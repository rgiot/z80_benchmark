from .base import Assembler
import abc
import os

PYZ80_MASTER_URL = "https://github.com/simonowen/pyz80/archive/refs/heads/master.zip"

class PyZ80(Assembler):
	"""Wrapper  to pyz80 assembler"""
	def __init__(self, base_location):
		super().__init__(base_location, "pyz80")
		self._inner_folder = "pyz80-master"

	def install(self):
		if self.check_install(): return
		
		super().install()
		self.unwrap_http_archive(PYZ80_MASTER_URL)
		ret = os.system("chmod +x %s" % self.exec_path())
		assert ret == 0


	def exec_path(self) -> str:
		return os.path.join(self.location(), self._inner_folder, "pyz80.py")
	