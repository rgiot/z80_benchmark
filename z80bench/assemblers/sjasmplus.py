from .base import Assembler
import os
import abc

SJASMPLUS_1_20_3_URL="https://github.com/z00m128/sjasmplus/archive/refs/tags/v1.20.3.zip"


class Sjasmplus(Assembler):
	"""Wrapper to sjasmplus.
	Maybe it is not fair to use it, I have not found how to save the result without adding OUTPUT directive to the source... so there is no save at all
	"""
	def __init__(self, base_location):
		super().__init__(base_location, "sjasmplus")
		self._inner_folder = "sjasmplus-1.20.3/"


	def install(self):
		if self.check_install(): return
		
		super().install()
		self.unwrap_http_archive(SJASMPLUS_1_20_3_URL)
		ret = os.system(f"cd \"{self.location()}/{self._inner_folder}/\" && make -j10 USE_LUA=0")
		assert ret == 0

	def exec_path(self) -> str:
		return os.path.join(self.location(), self._inner_folder, "sjasmplus")
	

	@abc.abstractmethod
	def build_cmd_line(self, ifname, ofname, includes):
		if includes:
			includes_arg = " ".join(f"-I\"{i}\""  for i in includes)
		else:
			includes_arg = ""
		return f"{self.exec_path()} \"{ifname}\" {includes_arg}"
