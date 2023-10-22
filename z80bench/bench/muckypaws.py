from z80bench.bench.source import Project
import os

class MuckyPawsProjectsGenerator(object):
	def __init__(self):
		self._dir = os.path.join(os.path.dirname(__file__), "..", "..", "z80", "muckypaws")

	def __iter__(self):
		for fname in [
			os.path.join(self._dir, "Assembly", "Tetris", "Tetris.asm"),
			os.path.join(self._dir, "Assembly", "Stix", "Stix.asm"),
			os.path.join(self._dir, "Assembly", "Jacelock", "Jacelock.asm"),
			os.path.join(self._dir, "Assembly", "Unknown", "Sniper.asm"),
			os.path.join(self._dir, "Commercial", "Nirvana", "NirvanaS48Copier.asm"),
			os.path.join(self._dir, "Commercial", "Misc", "AllHandX.asm"),
		]:
			yield Project(fname)
		