<?php
header('INTELLIGEN-2009: UpdateSystem');

error_reporting(0);

define('__ROOT__', dirname(dirname(__FILE__)));

require_once(__ROOT__.'/common/security.php');

if(SecuritySystem::IsIntelligeNClient())
{
	switch ($_REQUEST['action'])
	{
		case 'update_v1':
			include('update_v1.inc.php');
			break;
		case 'update_v2':
			include('update_v2.inc.php');
			break;

		case 'upload_v2':
			include('upload_v2.inc.php');
			break;
	}
}
else
{
	SecuritySystem::Fake404();
}
?>