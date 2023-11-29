from z80bench.bench.source import Project
import os
import glob

class Z80CryptosProjectsGenerator(object):
	def __init__(self):
		self._dir = os.path.join(os.path.dirname(__file__), "..", "..", "z80", "z80_crypto")
		self._relative_includes = ["."]

	def __iter__(self):
		for fname in glob.glob(os.path.join(self._dir, "*_test.asm")):
			yield Project(fname, [os.path.join(self._dir, i) for i in self._relative_includes])
		