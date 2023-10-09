import itertools
import numpy as np


from z80bench.bench.source import Project, Sources

from .assemblers.base import Assembler, AssemblingResult
from .bench.assemblers import Assemblers


class Bench(object):
	def __init__(self, out_dir: str, assemblers: Assemblers, sources: Sources, repetition=100):
		self.out_dir: str = out_dir
		self._assemblers: Assemblers = assemblers
		self._sources: Sources = sources

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

	def run(self):
		print("> Run benchmark")
		res = dict()

		# launch computation
		for repet in range(100):
			for (asm, src) in itertools.product(self._assemblers.assemblers(), self._sources.projects()):
				i_res = self.one_execution(asm, src)

				if src not in res:
					res[src] = dict()
				
				if asm not in res[src]:
					res[src][asm] = list()

				res[src][asm].append(i_res)
							
		# make stats
		for source, results in res.items():
			print(f"\n### `{source.name()}`")
			lines = []
			speed = []
			for i, (asm, values) in enumerate(results.items()):
				nb_errors = len([1 for res in values if res.is_err()])
				nb_tries = len(values)
				timings = [res.duration() for res in values if res.is_ok()]

				if nb_errors > 0:
					lines.append("- %-10s: unable to assemble" % (asm.flavor()) )
					speed.append(np.inf)
				else:

					mean = np.mean(timings)
					std = np.std(timings)

					lines.append("- %-10s: %f(%f)" % (asm.flavor(), mean, std) )
					speed.append(mean)
			best_idx = np.argmin(speed)
			lines[best_idx] += " * " # add the marker to the best
			print("\n".join(lines)) # draw the results

