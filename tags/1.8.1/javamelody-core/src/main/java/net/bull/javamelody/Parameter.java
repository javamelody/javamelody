/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import java.util.Locale;

/**
 * Liste des paramètres, tous optionnels; voir fichier README.txt pour plus de détails.
 * @author Emeric Vernat
 */
public enum Parameter {
	/**
	 * Résolution des courbes en secondes (60 par défaut).
	 * Une résolution entre 60 et 600 est recommandée (c'est-à-dire 1 à 10 minutes).
	 */
	RESOLUTION_SECONDS("resolution-seconds"),
	/**
	 * Nom du répertoire de stockage (monitoring par défaut).
	 * Si le nom du répertoire commence par '/', on considère que c'est un chemin absolu,
	 * sinon on considère que c'est un chemin relatif par rapport au répertoire temporaire
	 * ('temp' dans TOMCAT_HOME pour tomcat).
	 */
	STORAGE_DIRECTORY("storage-directory"),

	/**
	 * Active le log des requêtes http (false par défaut).
	 */
	LOG("log"),

	/**
	 * Seuil en millisecondes pour décompte en niveau warning (moyenne globale + 1 écart-type par défaut).
	 */
	WARNING_THRESHOLD_MILLIS("warning-threshold-millis"),

	/**
	 * Seuil en millisecondes pour décompte en niveau severe (moyenne globale + 2 * écart-type par défaut).
	 */
	SEVERE_THRESHOLD_MILLIS("severe-threshold-millis"),

	/**
	 * Expression régulière pour exclure certaines urls du monitoring (null par défaut).
	 * Voir {@link java.util.regex.Pattern http://java.sun.com/javase/6/docs/api/java/util/regex/Pattern.html}
	 */
	URL_EXCLUDE_PATTERN("url-exclude-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer la description de la requête http
	 * et pour supprimer des parties variables (identifiant d'objet par exemple)
	 * afin de permettre l'agrégation sur ces requêtes.
	 */
	HTTP_TRANSFORM_PATTERN("http-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer la description de la requête sql
	 * (identifiants non bindés dans une clause in par exemple)
	 * afin de permettre l'agrégation sur ces requêtes.
	 */
	SQL_TRANSFORM_PATTERN("sql-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer la description d'une méthode ejb3.
	 */
	EJB_TRANSFORM_PATTERN("ejb-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer la description d'une méthode spring.
	 */
	SPRING_TRANSFORM_PATTERN("spring-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer la description d'une méthode de
	 * façade autre que ejb3 ou spring (ie par MonitoringProxy).
	 */
	SERVICES_TRANSFORM_PATTERN("services-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer la description d'une erreur http.
	 */
	ERROR_TRANSFORM_PATTERN("error-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer la description d'un log.
	 */
	LOG_TRANSFORM_PATTERN("log-transform-pattern"),

	/**
	 * Compteurs affichés: mettre "http,sql,ejb" pour afficher les ejb3 ("http,sql" par défaut).
	 */
	DISPLAYED_COUNTERS("displayed-counters"),

	/**
	 * Active les actions Ramasse-miettes, Invalidation sessions et Heap-dump (false par défaut).
	 */
	SYSTEM_ACTIONS_ENABLED("system-actions-enabled"),

	/**
	 * Expression régulière (null par défaut) pour restreindre l'accès au monitoring à certaines adresses IP.
	 */
	ALLOWED_ADDR_PATTERN("allowed-addr-pattern"),

	/**
	 * Désactive le monitoring (false par défaut).
	 */
	DISABLED("disabled"),

	/**
	 * Liste des datasources jdbc quand elles ne peuvent trouvées automatiquement dans JNDI (null par défaut).
	 */
	DATASOURCES("datasources"),

	/**
	 * Nom JNDI de la session mail pour l'envoi par mail de rapport de hebdomadaire (null par défaut).
	 */
	MAIL_SESSION("mail-session"),

	/**
	 * Liste des adresses mails séparées par des virgules des destinataires
	 * pour l'envoi par mail de rapport de hebdomadaire (null par défaut).
	 */
	ADMIN_EMAILS("admin-emails"),

	/**
	 * Format du transport entre un serveur de collecte et une application monitorée
	 * (serialized : sérialisation java par défaut et recommandée pour les performances, xml : possible).
	 * <br/>Selon http://code.google.com/p/thrift-protobuf-compare/wiki/Benchmarking?ts=1237772203&updated=Benchmarking,
	 * la sérialisation java est 75% plus performante en temps que xml (xstream/xpp)
	 * et à peine plus gourmande en taille de flux.
	 */
	TRANSPORT_FORMAT("transport-format"),

	/**
	 * Identifiant de suivi google analytics s'il y a lieu (désactivé par défaut).
	 */
	ANALYTICS_ID("analytics-id"),

	/**
	 * Expérimental, ne pas utiliser.
	 */
	CONTEXT_FACTORY_ENABLED("context-factory-enabled");

	private final String code;

	private Parameter(String code) {
		this.code = code;
	}

	/**
	 * @return code de l'enum tel qu'il doit être paramétré
	 */
	public String getCode() {
		return code;
	}

	static Parameter valueOfIgnoreCase(String parameter) {
		return valueOf(parameter.toUpperCase(Locale.getDefault()).trim());
	}
}
