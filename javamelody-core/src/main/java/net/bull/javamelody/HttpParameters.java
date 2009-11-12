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

/**
 * Paramètres des requêtes http.
 * @author Emeric Vernat
 */
final class HttpParameters {
	static final String HTML_CHARSET = "ISO-8859-1";
	static final String HTML_CONTENT_TYPE = "text/html; charset=ISO-8859-1";
	static final String ACTION_PARAMETER = "action";
	static final String PART_PARAMETER = "part";
	static final String PERIOD_PARAMETER = "period";
	static final String SESSION_ID_PARAMETER = "sessionId";
	static final String COLLECTOR_PARAMETER = "collector";
	static final String REQUEST_PARAMETER = "request";
	static final String COUNTER_PARAMETER = "counter";
	static final String GRAPH_PARAMETER = "graph";
	static final String RESOURCE_PARAMETER = "resource";
	static final String FORMAT_PARAMETER = "format";
	static final String WIDTH_PARAMETER = "width";
	static final String HEIGHT_PARAMETER = "height";
	static final String HEAP_HISTO_PART = "heaphisto";
	static final String PROCESSES_PART = "processes";
	static final String CURRENT_REQUESTS_PART = "currentRequests";
	static final String WEB_XML_PART = "web.xml";
	static final String POM_XML_PART = "pom.xml";
	static final String SESSIONS_PART = "sessions";
	static final String DATABASE_PART = "database";
	static final String GRAPH_PART = "graph";

	/**
	 * Constructeur privé: pas d'instance.
	 */
	private HttpParameters() {
		super();
	}
}
