<?php
require_once('icms_const.php');
require_once('icms_utils.php');
include('icms_modul.php');

if(isset($_REQUEST['action']) && ($_REQUEST['action'] == 'add_entry'))
{
	$use_account = false;
	if(isset($_POST['account_username']) && ($_POST['account_username'] != '') &&
			isset($_POST['account_password']))
	{
		$use_account = true;
		
		$account_username = $_POST['account_username'];
		$account_password = $_POST['account_password'];
	}
	
	if(isset($_POST['MirrorCount']) && ($_POST['MirrorCount'] > 0))
	{
		$mirrorcount = $_POST['MirrorCount'];
		$directlinks = array();
		$crypterlinks = array();
		for ($i=0;$i<$mirrorcount;$i++)
		{
			$directlinksmirrorcount = $_POST['Mirror_' . $i . '_DirectlinksMirrorCount'];
			$directlinksmirror = array();
			for ($j=0;$j<$directlinksmirrorcount;$j++)
				$directlinksmirror[$j] = $_POST['Mirror_' . $i . '_DirectlinksMirror_' . $j];
		
			$directlinks[$i] = $directlinksmirror;
			
			$crypterlinksmirrorcount = $_POST['Mirror_' . $i . '_CrypterCount'];
			$crypterlinksmirror = array();
			for ($j=0;$j<$crypterlinksmirrorcount;$j++)
			{
				$crypterlinksmirrorinfo = array();
				$crypterlinksmirrorinfo['name'] = $_POST['Mirror_' . $i . '_Crypter_' . $j . '_Name'];
				$crypterlinksmirrorinfo['link'] = $_POST['Mirror_' . $i . '_Crypter_' . $j . '_Link'];
				$crypterlinksmirrorinfo['size'] = $_POST['Mirror_' . $i . '_Crypter_' . $j . '_Size'];
				$crypterlinksmirrorinfo['hoster'] = $_POST['Mirror_' . $i . '_Crypter_' . $j . '_Hoster'];	
				
				$crypterlinksmirror[$j] = $crypterlinksmirrorinfo;
			}
			
			$crypterlinks[$i] = $crypterlinksmirror;
		};
		
		if(isset($_POST['IType']) && in_array($_POST['IType'],$StringTemplateTypeID))
		{
			$type = $_POST['IType'];
			$subject = $_POST['ISubject'];
			
			$values = array();
			foreach($StringComponentID as $value) 
			{
				if(isset($_POST[$value]))
				{
					$values[$value] = $_POST[$value];
				}
			}
			unset($value);
			
			AddEntry($type, $subject, $values, $directlinks, $crypterlinks, $use_account, $account_username, $account_password);
			
			unset($type);
			unset($subject);
			unset($values);
			unset($use_account);
			unset($account_username);
			unset($account_password);
		}
		else
		{
			xmlresult(false,'IType is invalid');
		}
	}
	else
	{
		xmlresult(false,'No downloadlinks given!');
	}
} 
else
{
	xmlresult(false,'No data send');
}

?>