from ..assemblers.base import Assembler

from typing import Dict, List, ValuesView
import sys

class Assemblers(object):
	def __init__(self, filter):
		self._assemblers: Dict[str, Assembler]= {}
		self._flavors: List[str] = []
		self._filters = filter

	def add_assembler(self, assembler: Assembler):
		flavor = assembler.flavor()
		assert flavor not in self._flavors, "%s is already present !" % flavor

		if self._filters is None or flavor in self._filters:
			self._flavors.append(flavor)
			self._assemblers[flavor] = assembler

	def assemblers(self) -> ValuesView[Assembler]:
		return self._assemblers.values()
	
	def versions(self):
		for a in self.assemblers():
			sys.stdout.write("- ")
			sys.stdout.flush()
			a.version()