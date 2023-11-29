from z80bench.bench.source import Project
import os

class Z80VmProjectsGenerator(object):
	def __init__(self):
		self._dir = os.path.join(os.path.dirname(__file__), "..", "..", "z80", "z80-vm")
		self._relative_includes = []

	def __iter__(self):
		yield Project(os.path.join(self._dir, "tiny", "main.asm"), [os.path.join(self._dir, i) for i in self._relative_includes])
		