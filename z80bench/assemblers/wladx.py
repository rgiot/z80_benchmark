from .base import Assembler
import abc
import os

WLADX_10_0_5_URL = "https://github.com/vhelin/wla-dx/archive/refs/tags/v10.5.zip"

class WlaDx(Assembler):
	"""Wrapper  to wla-dx assembler"""
	def __init__(self, base_location):
		super().__init__(base_location, "wladx")
		self._inner_folder = "wla-dx-10.5"

	def install(self):
		if self.check_install(): return
		
		super().install()
		self.unwrap_http_archive(WLADX_10_0_5_URL)
		ret = os.system(f"cd \"{self.location()}/{self._inner_folder}/\" && cmake -G \"Unix Makefiles\" && make -j10")
		assert ret == 0

	def exec_path(self) -> str:
		p = os.path.join(self.location(), self._inner_folder, "binaries", "wla-z80")
		return p
	

	def build_cmd_line(self, ifname,  ofname, includes):
		if includes:
			includes_arg = " ".join(f"-I\"{i}\""  for i in includes)
		else:
			includes_arg = ""
		return f"{self.exec_path()} {includes_arg} -o \"{ofname}\" \"{ifname}\"  "