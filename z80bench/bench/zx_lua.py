from z80bench.bench.source import Project
import os

class ZxLuaProjectsGenerator(object):
	def __init__(self):
		self._dir = os.path.join(os.path.dirname(__file__), "..", "..", "z80", "zx_lua")

	def __iter__(self):
		for fname in [
			os.path.join(self._dir, "userlua.asm"),
		]:
			yield Project(fname)
		