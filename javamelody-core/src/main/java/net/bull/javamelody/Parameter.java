/*
 * Copyright 2008-2017 by Emeric Vernat
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

import net.bull.javamelody.internal.common.Parameters;

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
	 * façade autre que ejb3, spring ou guice (ie par {@link MonitoringProxy}).
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
	 * Active les actions systèmes telles que Ramasse-miettes, Invalidation sessions et Heap-dump (true par défaut).
	 */
	SYSTEM_ACTIONS_ENABLED("system-actions-enabled"),

	/**
	 * Active la protection contre CSRF (false par défaut).
	 */
	CSRF_PROTECTION_ENABLED("csrf-protection-enabled"),

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
	 * Désactive la vérification de l'authentification sur la page du monitoring dans le plugin Jenkins
	 * ou dans le plugin JIRA/Confluence/Bamboo, de manière à pouvoir utiliser le serveur de collecte
	 * centralisé (false par défaut). Le paramètre allowed-addr-pattern pourra être utilisé pour
	 * n'autoriser que le serveur de collecte pour l'obtention des données dans Jenkins/JIRA/Confluence/Bamboo.
	 */
	PLUGIN_AUTHENTICATION_DISABLED("plugin-authentication-disabled"),

	/**
	 * Désactive l'update-check (false par défaut).
	 */
	UPDATE_CHECK_DISABLED("update-check-disabled"),

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
	 * URL http du fichier jar javamelody-swing (null par défaut : le fichier sera téléchargé depuis https://github.com/javamelody/javamelody selon la version). <br/>
	 * Ce paramètre est surtout utile quand les utilisateurs n'ont pas accès à Internet pour télécharger sur github.
	 */
	JAVAMELODY_SWING_URL("javamelody-swing-url"),

	/**
	 * Name of a class to use for JavaMelody logs (default: null for detection of Logback, Log4J or java.util.logging). <br/>
	 * The class must implement the interface {@link net.bull.javamelody.JavaMelodyLogger},
	 * such as net.bull.javamelody.internal.common.Log4JLogger, net.bull.javamelody.internal.common.Log4J2Logger,
	 * net.bull.javamelody.internal.common.JavaLogger or net.bull.javamelody.internal.common.LogbackLogger.
	 */
	LOGGER_CLASS("logger-class"),

	/**
	 * Expose counters as JMX mbeans. Disabled by default.
	 */
	JMX_EXPOSE_ENABLED("jmx-expose-enabled"),

	/**
	 * Explicit name of the monitored application (in case the automatic name detection fails somehow).
	 */
	APPLICATION_NAME("application-name"),

	/**
	 * Explicit version of the monitored application (to override the version from Maven files for example).
	 */
	APPLICATION_VERSION("application-version"),

	/**
	 * Comma separated list of Maven repositories URLs (https://repo1.maven.org/maven2/ by default).
	 */
	MAVEN_REPOSITORIES("maven-repositories"),

	/**
	 * If Real User Monitoring enabled by injecting Boomerang javascript into html page ("false" by default).
	 */
	RUM_ENABLED("rum-enabled"),

	/**
	 * Address of the <a href='http://graphiteapp.org/'>Graphite</a> server to send metrics to,
	 * for example: 11.22.33.44:2003 (null by default).
	 */
	GRAPHITE_ADDRESS("graphite-address"),

	/**
	 * Address of the <a href='https://github.com/etsy/statsd'>StatsD</a> server to send metrics to,
	 * for example: 11.22.33.44:8125 (null by default).
	 */
	STATSD_ADDRESS("statsd-address"),

	/**
	 * Namespace to use in <a href='https://aws.amazon.com/cloudwatch/'>AWS CloudWatch</a> to send metrics,
	 * for example "MyCompany/MyAppDomain" (null by default).
	 */
	CLOUDWATCH_NAMESPACE("cloudwatch-namespace"),

	/**
	 * URL of the <a href='https://www.influxdata.com/time-series-platform/'>InfluxDB</a> server to send metrics to,
	 * for example: http://11.22.33.44:8086/write?db=mydb (null by default).
	 */
	INFLUXDB_URL("influxdb-url"),

	/**
	 * API key of the <a href='https://www.datadoghq.com/'>Datadog</a> to send metrics,
	 * for example: 9775a026f1ca7d1c6c5af9d94d9595a4 (null by default).
	 */
	DATADOG_API_KEY("datadog-api-key"),

	/**
	 * Includes last values of graphs when sending metrics to <a href='https://prometheus.io/'>Prometheus</a> (false by default).
	 */
	PROMETHEUS_INCLUDE_LAST_VALUE("prometheus-include-last-value");

	private final String code;

	Parameter(String code) {
		this.code = code;
	}

	/**
	 * @return code de l'enum tel qu'il doit être paramétré
	 */
	public String getCode() {
		return code;
	}

	/**
	 * @return valeur du paramètre
	 */
	public String getValue() {
		return Parameters.getParameterValue(this);
	}

	/**
	 * @return valeur du paramètre
	 */
	public boolean getValueAsBoolean() { // NOPMD
		return Boolean.parseBoolean(getValue());
	}

	/**
	 * Définit la valeur d'un paramètre en tant que propriété système.
	 * @param value Valeur
	 */
	public void setValue(String value) {
		assert value != null;
		System.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + getCode(), value);
	}

	static Parameter valueOfIgnoreCase(String parameter) {
		return valueOf(parameter.toUpperCase(Locale.ENGLISH).trim());
	}
}
