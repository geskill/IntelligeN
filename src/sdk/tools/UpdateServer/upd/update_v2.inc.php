<?php
require_once(__ROOT__ . '/common/security.php');
require_once(__ROOT__ . '/sql/index.php');
require_once(__ROOT__ . '/xml/index.php');
require_once(__ROOT__ . '/xml/message.php');

class UpdateSystem
{
    private function VersionCheck($major_version, $minor_version, $major_build, $minor_build)
    {
        return ($major_version == 2) && ($minor_version >= 129);
    }

    private function Error($code, $msg)
    {
        echo status_message(0, $code, $msg);
    }

    function __construct()
    {
        if (SecuritySystem::IsUpdateClient()) {

            // Information from IntelligeN.exe
            // i.e. 2.129.0.4
            //      2.129.1.0
            $major_version = $_REQUEST['major_version'];
            $minor_version = $_REQUEST['minor_version'];
            $major_build = $_REQUEST['major_build'];
            $minor_build = $_REQUEST['minor_build'];

            if (!$this->VersionCheck($major_version, $minor_version, $major_build, $minor_build)) {

                $this->Error(2, 'Unsupported version');
            } else {

                $sqlsystem = new SQLSystem();
                $sqlsystem->VersionRequest($major_version, $minor_version, $major_build, $minor_build);
            }
        } else {

            $this->Error(1, 'Unsupported client');
        }
    }
}

new UpdateSystem();
?>