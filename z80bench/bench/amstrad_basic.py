from z80bench.bench.source import Project
import os
import itertools

class AmstradBasicProjectsGenerator(object):
	def __init__(self):
		self._dir = os.path.join(os.path.dirname(__file__), "..", "..", "z80", "AmstradBasic")
		self._relative_includes = []

	def __iter__(self):
		yield Project(os.path.join(self._dir, "Main.asm"), [os.path.join(self._dir, i) for i in self._relative_includes])
		