<?php
ini_set('display_errors',FALSE);
parse_str(implode('&', array_slice($argv, 1)), $_GET);
$target=$_GET['target'];
$curl=$_GET['curl'];
if (!$target) {
	echo "";
	exit;
}
if ($curl) {
	//init curl
	$ch = curl_init();
	//curl_setopt可以設定curl參數
	//設定url
	curl_setopt($ch , CURLOPT_URL , $target);

	//cURL follow a redirect, use:
	curl_setopt($ch, CURLOPT_FOLLOWLOCATION, true);
	//顯示Http Header資訊
	//curl_setopt($ch , CURLOPT_HEADER, true);
	curl_setopt($ch, CURLOPT_HTTPHEADER, array('Accept: Accept:text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8', 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_5) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11'));
	$redirectURL = curl_getinfo($ch, CURLINFO_EFFECTIVE_URL );

	curl_setopt($ch, CURLOPT_HEADER, false);
	curl_setopt($ch, CURLOPT_USERAGENT,'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.1.13) Gecko/20080311 Firefox/2.0.0.13');
	curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
	curl_setopt($ch, CURLOPT_BINARYTRANSFER, true);
	curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);
	curl_setopt($ch, CURLOPT_FOLLOWLOCATION, true);
	curl_setopt($ch, CURLOPT_CONNECTTIMEOUT ,0); 
	curl_setopt($ch, CURLOPT_TIMEOUT, 60);

	//執行，並將結果存回
	$result = curl_exec($ch);
	//關閉連線
	curl_close($ch);
} else {
	$result=$target;
}
if ($result) {
	//$result=iconv( "big5" , "utf-8" , $result );
	$result=mb_convert_encoding($result,"utf-8","big5");
}
echo $result;

?>