<?php
require_once(__ROOT__.'/sql/data.inc.php');

require_once(__ROOT__.'/xml/message.php');


class SQLVersionResult {
	public $id;
	public $new = false;
}

class SQLSystem
{
	private $link;
	
	function __construct() {
		$this->link = mysql_connect(SQL_SERVER, SQL_LOGIN_USERNAME, SQL_LOGIN_PASSWORD);
		if (!$this->link) {
			die(message(0, 0, mysql_error()));
		}
		
		$db_selected = mysql_select_db(SQL_DATABASE, $this->link);
		if (!$db_selected) {
			die(message(0, 0, mysql_error()));
		}
	}
	
	function last_active_version()
	{
		return null;
	}
	
	function search_version($major_version, $minor_version, $major_build, $minor_build)
	{
		$result = new SQLVersionResult();
		
		$sql = "SELECT id FROM `int2k9_upd_version` WHERE";
		
		$sql .= " (`major_version_number` = '" . mysql_real_escape_string($major_version) . "')";
		
		$sql .= " AND (`minor_version_number` = '" . mysql_real_escape_string($minor_version) . "')";
		
		$sql .= " AND (`major_build_number` = '" . mysql_real_escape_string($major_build) . "')";
		
		if ($major_build = 0)
			$sql .= " AND (`minor_build_number` = '" . mysql_real_escape_string($minor_build) . "')";
		
		$query = mysql_query($sql, $this->link);
		if(!$query)
			die(message(0, 0, mysql_error()));
		
		if(mysql_num_rows($query) == 1)
		{
			$data = mysql_fetch_assoc($query);
			$result->id = $data['id'];
		}
		else
		{
			$result->new = true;
			
			$sql = "INSERT INTO `int2k9_upd_version`(`major_version_number`, `minor_version_number`, `major_build_number`, `minor_build_number`, `description`)";
			$sql .= " VALUES (" . mysql_real_escape_string($major_version) . ", " . mysql_real_escape_string($minor_version) . ", " . mysql_real_escape_string($major_build) . ", " . mysql_real_escape_string($minor_build) . ", '')";
			
			$query = mysql_query($sql);
			
			if($query)
				$result->id = mysql_insert_id($this->link);
			else
				die(message(0, 0, mysql_error()));
		}

		return $result;
	}
	
	function __destruct() {
		if(is_resource($this->link))
			mysql_close($this->link);
	}
}
?>