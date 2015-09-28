<?php
require_once(__ROOT__.'/common/utils.php');
require_once(__ROOT__.'/xml/index.php');

function message($status, $code, $msg) {
	$xml = new XML();
	XML::addElement($xml->rootnode, 'status', booltostr($status));
	XML::addElement($xml->rootnode, 'code', $code);
	XML::addElement($xml->rootnode, 'msg', $msg);
	return $xml;
}

?>