<?php

require_once(__ROOT__.'/sql/data.inc.php');

class MyDom{
	public static function addElement($apChild,$name,$value=null){
		$doc = $apChild->ownerDocument;
		if($apChild instanceof DomDocument)
			$doc = $apChild;
		
		if(is_null($value))
			$elm = $doc->createElement($name);
		else
			$elm = $doc->createElement($name,$value);
		
		$apChild->appendChild($elm);
		return $elm;
	}
	
	public static function addAttribute($apChild,$name,$value=null){
		$doc = $apChild->ownerDocument;
		if($apChild instanceof DomDocument)
			$doc = $apChild;
		
		$newAttribute = $doc->createAttribute($name);
		$apChild->appendChild($newAttribute);
		
		if(is_null($value))
			$elm = $doc->createTextNode("");
		else
			$elm = $doc->createTextNode($value);
		
		$newAttribute->appendChild($elm);
		return $elm;
	}	
}

mysql_connect(SQL_SERVER, SQL_LOGIN_USERNAME, SQL_LOGIN_PASSWORD);
mysql_select_db(SQL_DATABASE);

// mysql_real_escape_string
$sql = "SELECT * FROM `intelligen_2k9_update_versions` WHERE (`active` = '1')
		AND (`minor_build` = '0')
		ORDER BY `minor_version` DESC, `major_build` DESC LIMIT 1";

$query = mysql_query($sql);

if(mysql_num_rows($query) == 1)
{
	$data = mysql_fetch_assoc($query);
	$last_id = $data['id'];
	$major_version = $data['major_version'];
	$minor_version = $data['minor_version'];
	$major_build = $data['major_build'];
	$minor_build = $data['minor_build'];
}

$document = new DomDocument('1.0', 'utf-8');
$document->formatOutput = true;
$rootnode = MyDom::addElement($document, 'xml');
$header = MyDom::addElement($rootnode, 'header');
MyDom::addElement($header, 'files_dir', 'upd/files/');
MyDom::addElement($header, 'major_version', $major_version);
MyDom::addElement($header, 'minor_version', $minor_version);
MyDom::addElement($header, 'major_build', $major_build);
MyDom::addElement($header, 'minor_build', $minor_build);

$sql = "SELECT
		`intelligen_2k9_update_version_files`.`id` AS `v_f_id`,
		`intelligen_2k9_update_version_files`.`version_id` AS `v_f_version_id`,
		`intelligen_2k9_update_version_files`.`file_id` AS `v_f_file_id`,
		`intelligen_2k9_update_version_files`.`created` AS `v_f_created`,
		`intelligen_2k9_update_version_files`.`modified` AS `v_f_modified`,

		`intelligen_2k9_update_system_files`.`id` AS `s_f_id`,
		`intelligen_2k9_update_system_files`.`system_id` AS `s_f_system_id`,
		`intelligen_2k9_update_system_files`.`size` AS `s_f_size`,
		`intelligen_2k9_update_system_files`.`major_version` AS `s_f_major_version`,
		`intelligen_2k9_update_system_files`.`minor_version` AS `s_f_minor_version`,
		`intelligen_2k9_update_system_files`.`major_build` AS `s_f_major_build`,
		`intelligen_2k9_update_system_files`.`minor_build` AS `s_f_minor_build`,
		`intelligen_2k9_update_system_files`.`size` AS `s_f_size`,
		`intelligen_2k9_update_system_files`.`checksum` AS `s_f_checksum`,

		`intelligen_2k9_update_systems`.`id` AS `s_id`,
		`intelligen_2k9_update_systems`.`filesystem_id` AS `s_filesystem_id`,
		`intelligen_2k9_update_systems`.`filesystem_id`+0 AS `s_filesystem_int_id`,
		`intelligen_2k9_update_systems`.`path_appendix` AS `s_path_appendix`,
		`intelligen_2k9_update_systems`.`name` AS `s_name`

		 FROM `intelligen_2k9_update_version_files`
		 INNER JOIN `intelligen_2k9_update_system_files` ON `intelligen_2k9_update_version_files`.`file_id` = `intelligen_2k9_update_system_files`.`id`
		 INNER JOIN `intelligen_2k9_update_systems` ON `intelligen_2k9_update_system_files`.`system_id` = `intelligen_2k9_update_systems`.`id`
		 WHERE (`intelligen_2k9_update_version_files`.`version_id` = '" . mysql_real_escape_string($last_id) . "')";

$query = mysql_query($sql);
$files = MyDom::addElement($rootnode, 'files');
while($data = mysql_fetch_assoc($query))
{
	$file = MyDom::addElement($files, 'file');
	MyDom::addAttribute($file, 'type', $data['s_filesystem_int_id']);
	MyDom::addAttribute($file, 'size', $data['s_f_size']);
	MyDom::addAttribute($file, 'csum', $data['s_f_checksum']);
	MyDom::addElement($file, 'name', $data['s_name']);
	MyDom::addElement($file, 'info', '');
}
print $document->saveXML();

mysql_close();
?>