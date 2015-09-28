<?php

function encrypt_value($value, $key)
{
	$key = md5($key);
	
	$iv = mcrypt_ecb(MCRYPT_RIJNDAEL_128, $key, pack('H*', 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'), MCRYPT_ENCRYPT);
	$aes = mcrypt_module_open(MCRYPT_RIJNDAEL_128, '', MCRYPT_MODE_CFB, '');
	mcrypt_generic_init($aes, $key, $iv);

	$result = base64_encode(mcrypt_generic($aes, $value));
	mcrypt_generic_deinit($aes);
	mcrypt_module_close($aes);
	
	return $result;
}

?>