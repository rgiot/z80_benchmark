from z80bench.bench.source import Project
import os
import itertools

class MetalGearProjectsGenerator(object):
	def __init__(self):
		self._dir = os.path.join(os.path.dirname(__file__), "..", "..", "z80", "MetalGear")
		self._relative_includes = ["logic"]

	def __iter__(self):
		yield Project(os.path.join(self._dir, "MetalGear.asm"), [os.path.join(self._dir, i) for i in self._relative_includes])
		