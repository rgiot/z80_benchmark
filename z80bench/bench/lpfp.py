from z80bench.bench.source import Project
import os
import itertools

class LpfpProjectsGenerator(object):
	def __init__(self):
		self._dir = os.path.join(os.path.dirname(__file__), "..", "..", "z80", "lpfp")

	def __iter__(self):
		return itertools.chain(
			self.test_projects(), 
			self.raytrace_projects()

		)
		
	def test_projects(self):
		filenames = [
			os.path.join(self._dir, "lpfp", "addtest.asm"),
			os.path.join(self._dir, "lpfp", "subtest.asm"),
		]
		for filename in filenames:
			yield Project(filename)
	

	def raytrace_projects(self):
		yield Project(os.path.join(self._dir,
							 "raytracing",
							 "spectrum.asm"
							 ))