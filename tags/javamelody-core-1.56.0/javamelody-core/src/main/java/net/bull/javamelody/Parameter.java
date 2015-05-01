/*
 * Copyright 2008-2014 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.bull.javamelody;

import java.util.Locale;

/**
 * Liste des paramètres, tous optionnels.
 * @author Emeric Vernat
 */
public enum Parameter {
	/**
	 * Résolution des courbes en secondes (60 par défaut).
	 * Une résolution entre 60 et 600 est recommandée (c'est-à-dire 1 à 10 minutes).
	 */
	RESOLUTION_SECONDS("resolution-seconds"),

	/**
	 * Nombre de jours avant qu'un fichier de graphique JRobin (extension .rrd) qui n'est plus utilisé,
	 * soit considéré comme obsolète et soit supprimé automatiquement, à minuit (90 par défaut, soit 3 mois).
	 */
	OBSOLETE_GRAPHS_DAYS("obsolete-graphs-days"),

	/**
	 * Nombre de jours avant qu'un fichier de statistiques (extension .ser.gz),
	 * soit considéré comme obsolète et soit supprimé automatiquement, à minuit (365 par défaut, soit 1 an).
	 */
	OBSOLETE_STATS_DAYS("obsolete-stats-days"),

	/**
	 * Période en secondes du sampling pour trouver les hotspots (null par défaut : pas de sampling).
	 * Une valeur de 10 est recommandée (c'est-à-dire 10 secondes) pour ne pas entraîner d'overhead,
	 * ce qui nécessitera plusieurs heures pour avoir des résultats significatifs.
	 * Cette période peut-être une valeur décimale comme 0.1 pour avoir des résultats plus rapidement,
	 * mais avec un plus grand overhead.
	 */
	SAMPLING_SECONDS("sampling-seconds"),

	/**
	 * Nom du répertoire de stockage (monitoring par défaut).
	 * Si le nom du répertoire commence par '/', on considère que c'est un chemin absolu,
	 * sinon on considère que c'est un chemin relatif par rapport au répertoire temporaire
	 * ('temp' dans TOMCAT_HOME pour tomcat).
	 */
	STORAGE_DIRECTORY("storage-directory"),

	/**
	 * Active le log des requêtes http au niveau INFO (false par défaut).
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
	 * Expression régulière (null par défaut) pour transformer la description d'une requête jpa.
	 */
	JPA_TRANSFORM_PATTERN("jpa-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer la description d'une méthode spring.
	 */
	SPRING_TRANSFORM_PATTERN("spring-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer la description d'une méthode guice.
	 */
	GUICE_TRANSFORM_PATTERN("guice-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer la description d'une méthode de
	 * façade autre que ejb3, spring ou guice (ie par MonitoringProxy).
	 */
	SERVICES_TRANSFORM_PATTERN("services-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer la description d'une action Struts 2.
	 */
	STRUTS_TRANSFORM_PATTERN("struts-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer la description d'une action JSF.
	 */
	JSF_TRANSFORM_PATTERN("jsf-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer la description d'une erreur http.
	 */
	ERROR_TRANSFORM_PATTERN("error-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer la description d'un log.
	 */
	LOG_TRANSFORM_PATTERN("log-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer le nom d'un job.
	 */
	JOB_TRANSFORM_PATTERN("job-transform-pattern"),

	/**
	 * Expression régulière (null par défaut) pour transformer le nom d'une page jsp.
	 */
	JSP_TRANSFORM_PATTERN("jsp-transform-pattern"),

	/**
	 * Compteurs affichés: mettre "http,sql,error,log,ejb" pour afficher les ejb3
	 * ou "http,sql,error,log,spring" pour afficher les beans spring ("http,sql,error,log" par défaut).
	 */
	DISPLAYED_COUNTERS("displayed-counters"),

	/**
	 * Liste des noms de rapports personnalisés (null par défaut).<br/>
	 */
	CUSTOM_REPORTS("custom-reports"),

	/**
	 * Liste de packages exclus du sampling
	 * ("java,sun,com.sun,javax,org.apache,org.hibernate,oracle,org.postgresql,org.eclipse" par défaut).
	 */
	SAMPLING_EXCLUDED_PACKAGES("sampling-excluded-packages"),

	/**
	 * List of packages to include for sampling, can be used if sampling-excluded-packages is null
	 * (null by default).
	 */
	SAMPLING_INCLUDED_PACKAGES("sampling-included-packages"),

	/**
	 * Paramètre pour désactiver les graphiques jdbc, le compteur sql et le monitoring de base de
	 * données ("false" par défaut).
	 */
	NO_DATABASE("no-database"),

	/**
	 * Paramètre pour désactiver le listener sur le scheduler par défaut de Quartz ("false" par défaut).
	 */
	QUARTZ_DEFAULT_LISTENER_DISABLED("quartz-default-listener-disabled"),

	/**
	 * Paramètre pour désactiver la compression des flux http ("false" par défaut).
	 */
	GZIP_COMPRESSION_DISABLED("gzip-compression-disabled"),

	/**
	 * Active les actions Ramasse-miettes, Invalidation sessions et Heap-dump (false par défaut).
	 */
	SYSTEM_ACTIONS_ENABLED("system-actions-enabled"),

	/**
	 * Expression régulière (null par défaut) pour restreindre l'accès au monitoring à certaines adresses IP.
	 */
	ALLOWED_ADDR_PATTERN("allowed-addr-pattern"),

	/**
	 * List of authorized users for BASIC auth, when you do no want to use a realm and "security-constraint" in web.xml.<br/>
	 * Format : user:password, one by line or separated by comma <br/>
	 * <pre>
	 * user1:pwd1, user2:pwd2
	 * user3:pwd3
	 * </pre>
	 */
	AUTHORIZED_USERS("authorized-users"),

	/**
	 * Désactive la vérification de l'authentification sur la page du monitoring dans le plugin Hudson/Jenkins
	 * ou dans le plugin JIRA/Confluence/Bamboo, de manière à pouvoir utiliser le serveur de collecte
	 * centralisé (false par défaut). Le paramètre allowed-addr-pattern pourra être utilisé pour
	 * n'autoriser que le serveur de collecte pour l'obtention des données dans Hudson/Jenkins/JIRA/Confluence/Bamboo.
	 */
	PLUGIN_AUTHENTICATION_DISABLED("plugin-authentication-disabled"),

	/**
	 * Désactive le monitoring (false par défaut).
	 */
	DISABLED("disabled"),

	/**
	 * Liste des datasources jdbc quand elles ne peuvent trouvées automatiquement dans JNDI (null par défaut).
	 */
	DATASOURCES("datasources"),

	/**
	 * Si Tomcat et si JNDI est utilisé pour les datasources, active le rewraping des
	 * datasources au lieu de faire du rebinding dans JNDI (false par défaut).
	 * Cela permet de monitorer les requêtes SQL dans le cas où les datasources sont récupérées une
	 * fois pour toute et que l'initialisation de JavaMelody ne peux être faite avant (hibernate par exemple).
	 * Cela est utilisé notamment dans le plugin Atlassian pour JIRA.
	 */
	REWRAP_DATASOURCES("rewrap-datasources"),

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
	 * Liste des périodes d'envoi des mails séparées par des virgules
	 * pour l'envoi par mail de rapport de hebdomadaire.
	 * Les périodes doivent être "day", "week" ou "month" ("week" par défaut).
	 */
	MAIL_PERIODS("mail-periods"),

	/**
	 * Format du transport entre un serveur de collecte et une application monitorée
	 * (serialized : sérialisation java par défaut et recommandée pour les performances, xml : possible).
	 * <br/>Selon http://code.google.com/p/thrift-protobuf-compare/wiki/Benchmarking?ts=1237772203&updated=Benchmarking,
	 * la sérialisation java est 75% plus performante en temps que xml (xstream/xpp)
	 * et à peine plus gourmande en taille de flux.
	 */
	TRANSPORT_FORMAT("transport-format"),

	/**
	 * To fix the locale of the reports (default to null, ie locale is given by the language of the browser).<br/>
	 * Example values: "en_US", "en", "fr_FR", "de_DE" or "pt_BR"
	 */
	LOCALE("locale"),

	/**
	 * URL du rapport de monitoring (/monitoring par défaut).
	 */
	MONITORING_PATH("monitoring-path"),

	/**
	 * Identifiant de suivi google analytics s'il y a lieu (désactivé par défaut).
	 */
	ANALYTICS_ID("analytics-id"),

	/**
	 * Expérimental, ne pas utiliser.
	 */
	CONTEXT_FACTORY_ENABLED("context-factory-enabled"),

	/**
	 * true | false, true will return localhost/127.0.0.1 for hostname/hostaddress, false will attempt dns lookup for hostname (default: false).
	 */
	DNS_LOOKUPS_DISABLED("dns-lookups-disabled"),

	/**
	 * true | false, true will disable opening stack-traces of jdbc connections (default: false).
	 */
	CONNECTIONS_STACK_TRACES_DISABLED("connections-stack-traces-disabled"),

	/**
	 * URL http du fichier jar javamelody-swing (null par défaut : le fichier sera téléchargé depuis http://javamelody.googlecode.com selon la version). <br/>
	 * Ce paramètre est surtout utile quand les utilisateurs n'ont pas accès à Internet pour télécharger sur googlecode.
	 */
	JAVAMELODY_SWING_URL("javamelody-swing-url"),

	/**
	 * Name of a class to use for JavaMelody logs (default: null for detection of Logback, Log4J or java.util.logging). <br/>
	 * The class must implement the interface net.bull.javamelody.JavaMelodyLogger, 
	 * such as net.bull.javamelody.Log4JLogger, net.bull.javamelody.JavaLogger or net.bull.javamelody.LogbackLogger.
	 */
	LOGGER_CLASS("logger-class");

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
		return valueOf(parameter.toUpperCase(Locale.ENGLISH).trim());
	}
}
