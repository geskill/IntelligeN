<?php

require_once(__ROOT__.'/crypt/index.php');
require_once(__ROOT__.'/sql/index.php');
require_once(__ROOT__.'/upd/data.inc.php');
require_once(__ROOT__.'/xml/index.php');
require_once(__ROOT__.'/xml/message.php');

class UploadSystem
{
	/**
	 * @return XML
     */
	private function getFTPServer()
	{
		return ftp_server_message(1, 1, "OK");
	}

	/**
	 * @return null|XML
     */
	private function getSystems()
	{
		$result = null;

		$sqlsystem = new SQLSystem();
		$systems = $sqlsystem->GetAllSystems();

		if (is_null($systems) || count($systems) == 0) {

			$result = status_message(0, 0, 'No file system yet defined.');
		}
		else {

			$result = systems_message(1, 1, "OK", $systems);
		}

		return $result;
	}

	/**
	 * @return null|XML
     */
	private function addSystems()
	{
		$result = null;

		if (isset($_REQUEST['systems'])) {

			$systems = array();

			foreach ($_REQUEST['systems'] as $System) {

				$system = new SQLUpdateSystem();

				$system->filesystem_id = $System['filesystem_id'];
				$system->path_appendix = $System['path_appendix'];
				$system->name = $System['name'];

				$systems[] = $system;
			}

			$sqlsystem = new SQLSystem();
			$success = $sqlsystem->AddSystems($systems);

			if ($success) {

				$result = status_message(1, 1, 'Systems have been added successfully.');
			}
			else  {

				$result = status_message(0, 0, 'Systems not added.');
			}
		}
		else {

			$result = status_message(0, 0, 'No systems specified.');
		}

		return $result;
	}

	/**
	 * @return null|XML
	 */
	private function getVersions()
	{
		$result = null;

		$sqlsystem = new SQLSystem();
		$versions = $sqlsystem->GetAllVersions();

		if (is_null($versions) || count($versions) == 0) {

			$result = status_message(0, 0, 'No version system yet defined.');
		}
		else {

			$result = versions_message(1, 1, "OK", $versions);
		}

		return $result;
	}

	/**
	 * @return null|XML
     */
	private function addVersion()
	{
		$result = null;

		if (isset($_REQUEST['major_version']) && isset($_REQUEST['minor_version']) && isset($_REQUEST['major_build']) && isset($_REQUEST['minor_build'])) {

			$sqlsystem = new SQLSystem();
			$version_id = $sqlsystem->AddVersion($_REQUEST['major_version'], $_REQUEST['minor_version'], $_REQUEST['major_build'], $_REQUEST['minor_build']);

			if ($version_id) {

				$result = version_message(1, 1, 'Version have been added successfully.', $version_id);
			}
			else  {

				$result = status_message(0, 0, 'Version not added.');
			}
		}
		else {

			$result = status_message(0, 0, 'No version specified.');
		}

		return $result;
	}

	/**
	 * @return null|XML
     */
	private function addFiles()
	{
		$result = null;

		if (isset($_REQUEST['files']) && isset($_REQUEST['version_id'])) {

			$files = array();

			foreach ($_REQUEST['files'] as $File) {

				$file = new SQLUpdateSystemFile();

				$file->id = 0; // Not in DB yet.
				$file->system_id = $File['system_id'];
				$file->major_version = $File['major_version'];
				$file->minor_version = $File['minor_version'];
				$file->major_build = $File['major_build'];
				$file->minor_build = $File['minor_build'];
				$file->size_compressed = $File['size_compressed'];
				$file->checksum = $File['checksum'];

				$files[] = $file;
			}

			$sqlsystem = new SQLSystem();
			$success = $sqlsystem->AddFiles($_REQUEST['version_id'], $files);

			if ($success) {

				$result = status_message(1, 1, 'Files have been added successfully.');
			}
			else  {

				$result = status_message(0, 0, 'Files not added.');
			}
		}
		else {

			$result = status_message(0, 0, 'No files specified.');
		}

		return $result;
	}

	/**
	 * @return null|XML
     */
	private function activateVersion()
	{
		$result = null;

		if (isset($_REQUEST['version_id'])) {

			$sqlsystem = new SQLSystem();
			$success = $sqlsystem->SetVersionActive($_REQUEST['version_id']);

			if ($success) {

				$result = status_message(1, 1, 'Version has been activated successfully.');
			}
			else  {

				$result = status_message(0, 0, 'Version not activated.');
			}
		}
		else {

			$result = status_message(0, 0, 'No version id specified.');
		}

		return $result;
	}

	/**
	 *
	 */
	function __construct()
	{
		if(isset($_REQUEST['access_token']) && ($_REQUEST['access_token'] == ACCESS_TOKEN))
		{
			if(isset($_REQUEST['upload']))
			{
				switch ($_REQUEST['upload'])
				{
					case 'get_ftp_server_v2':
						echo $this->getFTPServer();
						break;
					case 'get_systems_v2':
						echo $this->getSystems();
						break;
					case 'add_systems_v2':
						echo $this->addSystems();
						break;

					case 'get_versions_v2':
						echo $this->getVersions();
						break;
					case 'add_version_v2':
						echo $this->addVersion();
						break;

					case 'add_files_v2':
						echo $this->addFiles();
						break;

					case 'activate_version_v2':
						echo $this->activateVersion();
						break;

					default:
						echo status_message(0, 0, 'The specified function does not exist.');
						break;
				}
			} else {
				echo status_message(0, 0, 'A upload function specification is required.');
			}
		} else {
			echo status_message(0, 0, 'The access token is invalid.');
		}
	}

	/**
	 *
	 */
	function __destruct() {

	}
}

new UploadSystem();
?>