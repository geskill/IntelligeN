<?php
require_once(__ROOT__.'/common/security.php');
require_once(__ROOT__.'/sql/index.php');
require_once(__ROOT__.'/xml/index.php');
require_once(__ROOT__.'/xml/message.php');

class UpdateSystem
{
	private $major_version, $minor_version, $major_build, $minor_build;
	private $sqlsystem;
	
	private function versioncheck() {
		return ($major_version == 2) && ($minor_version >= 129);
	}
	
	private function error($code, $msg) {
		echo message(0, $code, $msg);
	}
	
	function __construct() {
		
		if (SecuritySystem::IsUpdateClient())
		{	
			$major_version = $_REQUEST['major_version'];
			$minor_version = $_REQUEST['minor_version'];
			$major_build = $_REQUEST['major_build'];
			$minor_build = $_REQUEST['minor_build'];
			
			if (!$this->versioncheck())
				$this->error(2, 'Unsupported version');
			else
			{
				$sqlsystem = new SQLSystem();
				echo $updatesystem->searchversions();
			}
		}
		else
			$this->error(1, 'Unsupported client');
	}
	
	function searchversions() {
		$data = $sqlsystem->versionrequest($minor_version);
		
		
		echo message(1, 1, 'No update available');
		
		echo message(1, 2, 'Update available');
		
		echo message(1, 3, 'Update and upgrade available');
		
		echo message(1, 4, 'Upgrade available');
	}
}

new UpdateSystem();
?>