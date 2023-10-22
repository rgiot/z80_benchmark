from z80bench.bench.source import Project
import os
import glob

class DotCommandsProjectsGenerator(object):
	def __init__(self):
		self._dir = os.path.join(os.path.dirname(__file__), "..", "..", "z80", "dot-commands")
		self._relative_includes = ["."]

	def __iter__(self):
		for fname in glob.glob(os.path.join(self._dir, "*.asm")):
			yield Project(fname, [os.path.join(self._dir, i) for i in self._relative_includes])
		