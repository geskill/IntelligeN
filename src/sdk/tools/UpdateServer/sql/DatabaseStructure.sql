-- phpMyAdmin SQL Dump
-- version 4.3.11
-- http://www.phpmyadmin.net
--
-- Host: 127.0.0.1
-- Erstellungszeit: 01. Okt 2015 um 16:41
-- Server-Version: 5.6.24
-- PHP-Version: 5.6.8

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Datenbank: `intelligen_2k9`
--
CREATE DATABASE IF NOT EXISTS `intelligen_2k9` DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE `intelligen_2k9`;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `intelligen_2k9_update_systems`
--
-- Erstellt am: 30. Sep 2015 um 22:53
--

CREATE TABLE IF NOT EXISTS `intelligen_2k9_update_systems` (
  `id` int(5) NOT NULL,
  `filesystem_id` enum('fsRoot','fsConfig','fsPlugins','fsSettings','fsCMS','fsCMSSubject','fsCMSMessage','fsSite','fsType') COLLATE utf8mb4_unicode_ci DEFAULT 'fsRoot' COMMENT 'Represents TFileSystem defined in uBase.pas',
  `path_appendix` varchar(200) COLLATE utf8mb4_unicode_ci NOT NULL,
  `name` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'Name of the file incl. file extension'
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- RELATIONEN DER TABELLE `intelligen_2k9_update_systems`:
--

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `intelligen_2k9_update_system_files`
--
-- Erstellt am: 30. Sep 2015 um 19:36
--

CREATE TABLE IF NOT EXISTS `intelligen_2k9_update_system_files` (
  `id` int(5) NOT NULL,
  `system_id` int(5) NOT NULL,
  `major_version_number` int(3) NOT NULL DEFAULT '2',
  `minor_version_number` int(3) NOT NULL,
  `major_build_number` int(3) NOT NULL,
  `minor_build_number` int(3) NOT NULL DEFAULT '0',
  `size` int(25) NOT NULL,
  `checksum` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- RELATIONEN DER TABELLE `intelligen_2k9_update_system_files`:
--   `system_id`
--       `intelligen_2k9_update_systems` -> `id`
--   `system_id`
--       `intelligen_2k9_update_systems` -> `id`
--

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `intelligen_2k9_update_versions`
--
-- Erstellt am: 30. Sep 2015 um 23:20
--

CREATE TABLE IF NOT EXISTS `intelligen_2k9_update_versions` (
  `id` int(5) NOT NULL,
  `active` tinyint(1) NOT NULL DEFAULT '0',
  `major_version_number` int(3) NOT NULL DEFAULT '2',
  `minor_version_number` int(3) NOT NULL,
  `major_build_number` int(3) NOT NULL,
  `minor_build_number` int(3) NOT NULL DEFAULT '0',
  `created` datetime NOT NULL,
  `modified` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- RELATIONEN DER TABELLE `intelligen_2k9_update_versions`:
--

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `intelligen_2k9_update_version_files`
--
-- Erstellt am: 30. Sep 2015 um 19:27
--

CREATE TABLE IF NOT EXISTS `intelligen_2k9_update_version_files` (
  `id` int(5) NOT NULL,
  `version_id` int(5) NOT NULL,
  `file_id` int(5) NOT NULL,
  `created` datetime NOT NULL,
  `modified` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- RELATIONEN DER TABELLE `intelligen_2k9_update_version_files`:
--   `version_id`
--       `intelligen_2k9_update_versions` -> `id`
--   `file_id`
--       `intelligen_2k9_update_system_files` -> `id`
--

--
-- Indizes der exportierten Tabellen
--

--
-- Indizes für die Tabelle `intelligen_2k9_update_systems`
--
ALTER TABLE `intelligen_2k9_update_systems`
  ADD PRIMARY KEY (`id`);

--
-- Indizes für die Tabelle `intelligen_2k9_update_system_files`
--
ALTER TABLE `intelligen_2k9_update_system_files`
  ADD PRIMARY KEY (`id`), ADD KEY `system_id` (`system_id`);

--
-- Indizes für die Tabelle `intelligen_2k9_update_versions`
--
ALTER TABLE `intelligen_2k9_update_versions`
  ADD PRIMARY KEY (`id`);

--
-- Indizes für die Tabelle `intelligen_2k9_update_version_files`
--
ALTER TABLE `intelligen_2k9_update_version_files`
  ADD PRIMARY KEY (`id`), ADD KEY `version_id` (`version_id`), ADD KEY `file_id` (`file_id`);

--
-- AUTO_INCREMENT für exportierte Tabellen
--

--
-- AUTO_INCREMENT für Tabelle `intelligen_2k9_update_systems`
--
ALTER TABLE `intelligen_2k9_update_systems`
  MODIFY `id` int(5) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=1;
--
-- AUTO_INCREMENT für Tabelle `intelligen_2k9_update_system_files`
--
ALTER TABLE `intelligen_2k9_update_system_files`
  MODIFY `id` int(5) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=1;
--
-- AUTO_INCREMENT für Tabelle `intelligen_2k9_update_versions`
--
ALTER TABLE `intelligen_2k9_update_versions`
  MODIFY `id` int(5) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=1;
--
-- AUTO_INCREMENT für Tabelle `intelligen_2k9_update_version_files`
--
ALTER TABLE `intelligen_2k9_update_version_files`
  MODIFY `id` int(5) NOT NULL AUTO_INCREMENT,AUTO_INCREMENT=1;
--
-- Constraints der exportierten Tabellen
--

--
-- Constraints der Tabelle `intelligen_2k9_update_system_files`
--
ALTER TABLE `intelligen_2k9_update_system_files`
ADD CONSTRAINT `intelligen_2k9_update_system_files_ibfk_1` FOREIGN KEY (`system_id`) REFERENCES `intelligen_2k9_update_systems` (`id`);

--
-- Constraints der Tabelle `intelligen_2k9_update_version_files`
--
ALTER TABLE `intelligen_2k9_update_version_files`
ADD CONSTRAINT `intelligen_2k9_update_version_files_ibfk_1` FOREIGN KEY (`version_id`) REFERENCES `intelligen_2k9_update_versions` (`id`),
ADD CONSTRAINT `intelligen_2k9_update_version_files_ibfk_2` FOREIGN KEY (`file_id`) REFERENCES `intelligen_2k9_update_system_files` (`id`);

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
