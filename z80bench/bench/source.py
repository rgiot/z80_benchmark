from typing import Iterable, List
from ..assemblers.base import Assembler
import os

class Project(object):
	def __init__(self, fname: str, includes: List[str] = None):
		self._fname = fname
		self._includes = includes

	def build_with(self, asm: Assembler):
		return asm.build(os.path.abspath(self._fname), self._includes)
	
	def name(self):
		return self._fname
	
	def __repr__(self) -> str:
		return self.name()


class Sources(object):
	def __init__(self) -> None:
		self._projects = []

	def add_project(self, proj: Project):
		self._projects.append(proj)

	def add_group(self, group: Iterable[Project]):
		for p in group:
			self.add_project(p)

	def projects(self):
		return self._projects