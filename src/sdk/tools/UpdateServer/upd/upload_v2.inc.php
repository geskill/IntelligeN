<?php

require_once(__ROOT__.'/crypt/index.php');
require_once(__ROOT__.'/sql/index.php');
require_once(__ROOT__.'/upd/data.inc.php');
require_once(__ROOT__.'/xml/index.php');
require_once(__ROOT__.'/xml/message.php');

function getFTPServer() {
	$xml = new XML();
	XML::addElement($xml->rootnode, 'status', 1);
	XML::addElement($xml->rootnode, 'code', 1);
	XML::addElement($xml->rootnode, 'msg', 'OK');
	$server = XML::addElement($xml->rootnode, 'server');
	
	$key = FTP_SERVER . ACCESS_TOKEN . FTP_SERVER;
	XML::addElement($server, 'name', FTP_SERVER);
	XML::addElement($server, 'port', encrypt_value(FTP_PORT, $key));
	XML::addElement($server, 'path', encrypt_value(FTP_PATH, $key));
	XML::addElement($server, 'username', encrypt_value(FTP_LOGIN_USERNAME, $key));
	XML::addElement($server, 'password', encrypt_value(FTP_LOGIN_PASSWORD, $key));
	
	return $xml;
}

function getVersions() {
	if(isset($_POST['major_version']) && isset($_POST['minor_version']) && isset($_POST['major_build']))
	{		
		$xml = new XML();
		XML::addElement($xml->rootnode, 'status', 1);
		XML::addElement($xml->rootnode, 'code', 1);
		XML::addElement($xml->rootnode, 'msg', 'OK');
		
		$xml_versions = XML::addElement($xml->rootnode, 'versions');		
		
		$sql = new SQLSystem();
		
		for ($i = 0, $rowcount = count($_POST['major_version']); $i < $rowcount; $i++)
		{			
			$major_version = $_POST['major_version'][$i];
			
			$minor_version = $_POST['minor_version'][$i];
			
			$major_build = $_POST['major_build'][$i];
			
			if ($major_build == 0) // for preview builds minor_build is interesting, too
				$minor_build = $_POST['minor_build'][$i];
			else
				$minor_build = 0;
			
			$xml_version = XML::addElement($xml_versions, 'version');
			
			if(($major_version == 2) && ($minor_version >= 129))
			{
				$versionresult = $sql->search_version($major_version, $minor_version, $major_build, $minor_build);
				
				XML::addAttribute($xml_version, 'id', $versionresult->id);
				XML::addAttribute($xml_version, 'new', $versionresult->new);
				
				unset($versionresult);
			}
			else
			{
				XML::addAttribute($xml_version, 'id', '-1');
			}
			
			XML::addElement($xml_version, 'major_version', $major_version);
			XML::addElement($xml_version, 'minor_version', $minor_version);
			XML::addElement($xml_version, 'major_build', $major_build);
			XML::addElement($xml_version, 'minor_build', $minor_build);			
		}
		
		unset($sql);
		
		return $xml;
	}
	else
		return message(0, 2, 'No versions send.');
}

function getFiles() {
	return "";
}

if(isset($_REQUEST['access_token']) && ($_REQUEST['access_token'] == ACCESS_TOKEN))
{
	if(isset($_REQUEST['upload']))
	{
		switch ($_REQUEST['upload'])
		{			
			case 'ftpserver_v2':
				echo getFTPServer();
				break;
			case 'versions_v2':
				echo getVersions();
				break;
			case 'files_v2':
				echo getFiles();
				break;
		}
	}
} else {
	echo message(0, 0, 'The access token is invalid.');
}
?>