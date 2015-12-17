<?php

class MyDom
{
	public static function addElement($apChild, $name, $value = null)
	{
		$doc = $apChild->ownerDocument;
		if ($apChild instanceof DomDocument)
			$doc = $apChild;

		if (is_null($value))
			$elm = $doc->createElement($name);
		else
			$elm = $doc->createElement($name, $value);

		$apChild->appendChild($elm);
		return $elm;
	}

	public static function addAttribute($apChild, $name, $value = null)
	{
		$doc = $apChild->ownerDocument;
		if ($apChild instanceof DomDocument)
			$doc = $apChild;

		$newAttribute = $doc->createAttribute($name);
		$apChild->appendChild($newAttribute);

		if (is_null($value))
			$elm = $doc->createTextNode('');
		else
			$elm = $doc->createTextNode($value);

		$newAttribute->appendChild($elm);
		return $elm;
	}
}

function BoolToStr($bValue = false)
{
	return ($bValue ? 'True' : 'False');
}

function xmlresult($code, $msg = '')
{
	$document = new DomDocument();

	$rootnode = MyDom::addElement($document, 'xml');

	MyDom::addElement($rootnode, 'code', BoolToStr($code));
	MyDom::addElement($rootnode, 'msg', $msg);

	print $document->saveXML();
}

?>