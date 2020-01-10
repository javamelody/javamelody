/*
 * Copyright 2008-2019 by Emeric Vernat
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

import javax.servlet.http.HttpServletRequest;

/**
 * Interface commune aux 3 implémentations de Loggers JavaMelody.
 * @author lastmike, Emeric Vernat
 */
public interface JavaMelodyLogger {
	/**
	 * Nom du logger interne.
	 */
	String INTERNAL_LOGGER_NAME = "net.bull.javamelody";

	/**
	 * Log interne en niveau debug.
	 * @param msg Message
	 */
	void debug(String msg);

	/**
	 * Log interne en niveau debug.
	 * @param msg Message
	 * @param throwable Throwable
	 */
	void debug(String msg, Throwable throwable);

	/**
	 * Log interne en niveau info.
	 * @param msg Message
	 */
	void info(String msg);

	/**
	 * Log interne en niveau info.
	 * @param msg Message
	 * @param throwable Throwable
	 */
	void info(String msg, Throwable throwable);

	/**
	 * Log interne en niveau warn.
	 * @param msg Message
	 * @param throwable Throwable
	 */
	void warn(String msg, Throwable throwable);

	/**
	 * Log les détails de l'exécution d'une requête http en niveau info.
	 * @param httpRequest Requête http
	 * @param requestName Nom de la requête
	 * @param duration Durée
	 * @param systemError Si erreur systême
	 * @param responseStatus Status de la réponse
	 * @param responseSize Taille de la réponse
	 * @param loggerName Nom du logger à utiliser
	 */
	void logHttpRequest(HttpServletRequest httpRequest, String requestName, long duration,
			boolean systemError, int responseStatus, long responseSize, String loggerName);
}
