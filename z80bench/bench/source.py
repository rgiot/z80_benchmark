from ..assemblers.base import Assembler
import os

class Project(object):
	def __init__(self, fname: str):
		self._fname = fname

	def build_with(self, asm: Assembler):
		return asm.build(os.path.abspath(self._fname))
	
	def name(self):
		return self._fname


class Sources(object):
	def __init__(self) -> None:
		self._projects = []

	def add_project(self, proj: Project):
		self._projects.append(proj)

	def projects(self):
		return self._projects