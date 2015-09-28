<?php

class SecuritySystem
{
	public static function IsIntelligeNClient() {
		return isset($_SERVER['SERVER_PROTOCOL']) && isset($_SERVER['HTTP_USER_AGENT']) && isset($_SERVER['REMOTE_ADDR']) && isset($_REQUEST['action']);
	}
	
	public static function IsUpdateClient() {
		return isset($_REQUEST['major_version']) && isset($_REQUEST['minor_version']) && isset($_REQUEST['major_build']) && isset($_REQUEST['minor_build']);
	}
	
	public static function Fake404() {
		sleep(3);
		header('HTTP/1.1 404 Not Found');
		header('Location: ./');
	}
}

?>