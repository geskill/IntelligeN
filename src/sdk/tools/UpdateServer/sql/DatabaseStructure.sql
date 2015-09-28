-- phpMyAdmin SQL Dump
-- version 3.4.3.1
-- http://www.phpmyadmin.net
--
-- Host: localhost:3306
-- Erstellungszeit: 06. Mrz 2013 um 19:27
-- Server Version: 5.5.27
-- PHP-Version: 5.3.16

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Datenbank: `geskill_intellig`
--
CREATE DATABASE `geskill_intellig` DEFAULT CHARACTER SET utf8 COLLATE utf8_unicode_ci;
USE `geskill_intellig`;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `int2k9_upd_files`
--

CREATE TABLE IF NOT EXISTS `int2k9_upd_files` (
  `id` int(4) NOT NULL AUTO_INCREMENT,
  `file_major_version_number` int(3) NOT NULL DEFAULT '2',
  `file_minor_version_number` int(3) NOT NULL,
  `file_major_build_number` int(3) NOT NULL,
  `file_minor_build_number` int(3) NOT NULL,
  `file_filesystem` int(1) NOT NULL,
  `file_pathappendix` varchar(200) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `file_name` varchar(100) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `file_size` int(25) NOT NULL,
  `file_checksum` varchar(32) CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  `file_info` text CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `int2k9_upd_version`
--

CREATE TABLE IF NOT EXISTS `int2k9_upd_version` (
  `id` int(4) NOT NULL AUTO_INCREMENT,
  `live` tinyint(1) NOT NULL DEFAULT '0',
  `major_version_number` int(3) NOT NULL DEFAULT '2',
  `minor_version_number` int(3) NOT NULL,
  `major_build_number` int(3) NOT NULL,
  `minor_build_number` int(3) NOT NULL,
  `description` text CHARACTER SET utf8 COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=2 ;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `int2k9_upd_version_files`
--

CREATE TABLE IF NOT EXISTS `int2k9_upd_version_files` (
  `id` int(3) NOT NULL AUTO_INCREMENT,
  `id_version` int(3) NOT NULL,
  `id_file` int(4) NOT NULL,
  `action` int(1) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
