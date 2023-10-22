import itertools
import numpy as np
import pandas as pd
import os


from z80bench.bench.source import Project, Sources

from .assemblers.base import Assembler, AssemblingResult
from .bench.assemblers import Assemblers


class Bench(object):
	def __init__(self, out_dir: str, assemblers: Assemblers, sources: Sources, repetition=100):
		self.out_dir: str = out_dir
		self._assemblers: Assemblers = assemblers
		self._sources: Sources = sources
		self._repeat = repetition

	def install(self):
		print("> Install assemblers")
		for assembler in self._assemblers.assemblers():
			print(f"> Install {assembler.flavor()}")
			assembler.install()

	def versions(self):
		print("> Collect assemblers version")
		self._assemblers.versions()


	def one_execution(self, assembler: Assembler, project: Project) -> AssemblingResult :
		return project.build_with(assembler)

	def run(self, filter):
		print("> Run benchmark")
		res = dict()

		# launch computation
		for _ in range(self._repeat):
			for (asm, src) in itertools.product(self._assemblers.assemblers(), self._sources.projects(filter)):
				i_res = self.one_execution(asm, src)

				if src not in res:
					res[src] = dict()
				
				if asm not in res[src]:
					res[src][asm] = list()

				res[src][asm].append(i_res)
							
		# make stats
		sources, flavors, mean, std, failures = [], [], [], [], []
		for source, results in res.items():
			for i, (asm, values) in enumerate(results.items()):
				sources.append(os.path.relpath(source.name()))
				nb_errors = len([1 for res in values if res.is_err()])
				nb_tries = len(values)
				failures.append(nb_errors * 100 /nb_tries)
				timings = [res.duration() for res in values if res.is_ok()]

				if nb_errors > 0:
					mean.append(np.nan)
					std.append(np.nan)
				else:
					mean.append(np.mean(timings))
					std.append(np.std(timings))

				flavors.append(asm.flavor())


		print(len(sources), len(flavors), len(mean), len(std), len(failures))

		df = pd.DataFrame.from_dict({
			'Source': sources,
			'Assembler': flavors,
			'Mean': mean,
			'Std': std,
			'Failing ratio': failures
		})

		def build_representation(row):
			print(row)
			if row['Failing ratio'] != 0:
				return "Failure."
			else:
				return "%0.3f (%0.3f)" % (row["Mean"], row["Std"])
		df["Speed"] = df.apply(build_representation, axis=1)

		table = df.pivot(values = "Speed", index = "Source", columns = "Assembler")
		success1 = table.apply(lambda val : val != "Failure.").mean()
		success2 = table.apply(lambda val : val != "Failure.").mean(axis=1)


		print("## Summary of assembling time")
		print(table.to_markdown())

		print("# Success rate over assemblers")
		print(success1.to_markdown())

		print("#Success rate over projects")
		print(success2.to_markdown())
