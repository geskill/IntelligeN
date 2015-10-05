<?php
require_once(__ROOT__.'/common/utils.php');
require_once(__ROOT__.'/upd/data.inc.php');
require_once(__ROOT__.'/xml/index.php');

/**
 * @param $status
 * @param $code
 * @param $msg
 * @return XML
 */
function status_message($status, $code, $msg) {

	$xml = new XML();

	XML::addElement($xml->rootnode, 'status', booltostr($status));
	XML::addElement($xml->rootnode, 'code', $code);
	XML::addElement($xml->rootnode, 'msg', $msg);

	return $xml;
}

/**
 * @param $status
 * @param $code
 * @param $msg
 * @param SQLUpdateVersionFile[]|null $upgrade
 * @param SQLUpdateVersionFile[]|null $update
 * @return XML
 */
function files_message($status, $code, $msg, $upgrade = null, $update = null) {

	$xml = status_message($status, $code, $msg);

	$upgrade_elm = XML::addElement($xml->rootnode, 'upgrade');
	add_files_to_files_message($upgrade_elm, $upgrade);

	$update_elm = XML::addElement($xml->rootnode, 'update');
	add_files_to_files_message($update_elm, $update);

	return $xml;
}

/**
 * @param $status
 * @param $code
 * @param $msg
 * @param SQLUpdateSystem[]|null $data
 * @return XML
 */
function systems_message($status, $code, $msg, $data = null) {

	$xml = status_message($status, $code, $msg);

	if (!is_null($data)) {

		$systems = XML::addElement($xml->rootnode, 'systems');

		foreach ($data as $System)  {

			$system = XML::addElement($systems, 'system');

			XML::addElement($system, 'id', $System->id);
			XML::addElement($system, 'filesystem_id', $System->filesystem_id);
			XML::addElement($system, 'path_appendix', $System->path_appendix);
			XML::addElement($system, 'name', $System->name);
		}
	}

	return $xml;
}

/**
 * @param int $status
 * @param int $code
 * @param $msg
 * @param int $version_id
 * @return XML
 */
function version_message($status, $code, $msg, $version_id) {

	$xml = status_message($status, $code, $msg);

	$version = XML::addElement($xml->rootnode, 'version');

	XML::addElement($version, 'id', $version_id);

	return $xml;
}

/**
 * @param $status
 * @param $code
 * @param $msg
 * @param SQLUpdateVersion[]|null $data
 * @return XML
 */
function versions_message($status, $code, $msg, $data = null) {

	$xml = status_message($status, $code, $msg);

	if (!is_null($data)) {

		$versions = XML::addElement($xml->rootnode, 'versions');

		foreach ($data as $Version)  {

			$version = XML::addElement($versions, 'version');

			XML::addElement($version, 'id', $Version->id);
			XML::addElement($version, 'active', $Version->active);
			XML::addElement($version, 'major_version', $Version->major_version);
			XML::addElement($version, 'minor_version', $Version->minor_version);
			XML::addElement($version, 'major_build', $Version->major_build);
			XML::addElement($version, 'minor_build', $Version->minor_build);
			XML::addElement($version, 'created', $Version->created);
			XML::addElement($version, 'modified', $Version->modified);
		}
	}

	return $xml;
}

/**
 * @param $status
 * @param $code
 * @param $msg
 * @return XML
 */
function ftp_server_message($status, $code, $msg) {

	$xml = status_message($status, $code, $msg);
	$server = XML::addElement($xml->rootnode, 'server');

	$key = FTP_SERVER . ACCESS_TOKEN . FTP_SERVER;
	XML::addElement($server, 'name', FTP_SERVER);
	XML::addElement($server, 'port', encrypt_value(FTP_PORT, $key));
	XML::addElement($server, 'path', encrypt_value(FTP_PATH, $key));
	XML::addElement($server, 'username', encrypt_value(FTP_LOGIN_USERNAME, $key));
	XML::addElement($server, 'password', encrypt_value(FTP_LOGIN_PASSWORD, $key));

	return $xml;
}

/**
 * @param $node
 * @param SQLUpdateVersionFile[]|null $data
 */
function add_files_to_files_message($node, $data) {

	if (!is_null($data)) {

		$header_defined = false;
		$header = XML::addElement($node, 'header');

		$files = XML::addElement($node, 'files');

		foreach ($data as $VersionFile)  {

			if (!$header_defined) {

				XML::addElement($header, 'files_dir', HTTP_PATH);

				XML::addElement($header, 'major_version', $VersionFile->version->major_version);
				XML::addElement($header, 'minor_version', $VersionFile->version->minor_version);
				XML::addElement($header, 'major_build', $VersionFile->version->major_build);
				XML::addElement($header, 'minor_build', $VersionFile->version->minor_build);
				XML::addElement($header, 'created', $VersionFile->version->created);
				XML::addElement($header, 'modified', $VersionFile->version->modified);

				$header_defined = true;
			}

			$file = XML::addElement($files, 'file');

			XML::addElement($file, 'major_version', $VersionFile->file->major_version);
			XML::addElement($file, 'minor_version', $VersionFile->file->minor_version);
			XML::addElement($file, 'major_build', $VersionFile->file->major_build);
			XML::addElement($file, 'minor_build', $VersionFile->file->minor_build);
			XML::addElement($file, 'size', $VersionFile->file->size);
			XML::addElement($file, 'checksum', $VersionFile->file->checksum);

			XML::addElement($file, 'filesystem_id', $VersionFile->file->system->filesystem_id);
			XML::addElement($file, 'path_appendix', $VersionFile->file->system->path_appendix);
			XML::addElement($file, 'name', $VersionFile->file->system->name);
		}
	}
}

?>