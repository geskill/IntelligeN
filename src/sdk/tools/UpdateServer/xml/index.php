<?php

class XML
{
	private $document;
	public $rootnode;

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

	function __construct() {
		$this->document = new DomDocument('1.0', 'utf-8');
		$this->document->formatOutput = true;
		$this->rootnode = XML::addElement($this->document, 'xml');
	}
	
	public function __toString() {
		return $this->document->saveXML();
	}
	
}


?>
