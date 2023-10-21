from z80bench.bench.source import Project
import os
import itertools

class MSXProjectsGenerator(object):
	def __init__(self):
		self._dir = os.path.join(os.path.dirname(__file__), "..", "..", "z80", "MSX", "SRC")

	def __iter__(self):
		return itertools.chain(
			self.crypto_projects(), 
			self.denyonet_projects(), 
			self.inl_projects(),
			self.misc_projects()
		)
		
	def denyonet_projects(self):
		return self.single_files_in("DENYONET")

	def crypto_projects(self):
		return self.single_files_in("CRYPTO")
	
	def inl_projects(self):
		return self.single_files_in("INL")
	
	def misc_projects(self):
		return self.single_files_in("MISC")


	def single_files_in(self, group):
		print(os.path.join(self._dir, group))
		for (dirpath, dirnames, filenames) in os.walk(os.path.join(self._dir, group)):
			for filename in filenames:
				if filename.endswith(".ASM"):
					yield Project(filename)
	