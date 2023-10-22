from z80bench.bench.source import Project
import os
import itertools

class Youkaiyashiki(object):
	def __init__(self):
		self._dir = os.path.join(os.path.dirname(__file__), "..", "..", "z80", "youkaiyashiki")

	def __iter__(self):
		yield Project(os.path.join(self._dir, "original", "YoukaiYashiki.asm"))
		