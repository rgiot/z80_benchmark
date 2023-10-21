from .base import Assembler
import abc
import os

RASM_2_0_SRC_URL = "https://github.com/EdouardBERGE/rasm/archive/refs/tags/v2.0.zip"

class Rasm(Assembler):
	"""Wrapper to RASM assembler"""
	def __init__(self, base_location):
		super().__init__(base_location, "rasm")
		self._inner_folder = "rasm-2.0"

	def install(self):
		if self.check_install(): return
		
		super().install()
		self.unwrap_http_archive(RASM_2_0_SRC_URL)
		ret = os.system(f"cd \"{self.location()}/{self._inner_folder}/\" && make -j10")
		assert ret == 0

	def exec_path(self) -> str:
		return os.path.join(self.location(), self._inner_folder, "rasm.exe")
	
	# rasm does not have a version option
	@abc.abstractmethod
	def version(self)-> str:
		print("""RASM v2.0 (build xx/07/2023) - Blue Hedgehog
(c) 2017 Edouard BERGE (use -n option to display all licenses / -autotest for self-testing)
LZ4 (c) Yann Collet / ZX0 & ZX7 (c) Einar Saukas / Exomizer 2 (c) Magnus Lind / LZSA & AP-Ultra (c) Emmanuel Marty""")
