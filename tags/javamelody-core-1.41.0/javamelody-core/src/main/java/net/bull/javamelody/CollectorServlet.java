/*
 * Copyright 2008-2012 by Emeric Vernat
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
package net.bull.javamelody; // NOPMD

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StreamCorruptedException;
import java.util.regex.Pattern;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * Servlet de collecte utilisée uniquement pour serveur de collecte séparé de l'application monitorée.
 * @author Emeric Vernat
 */
public class CollectorServlet extends HttpServlet {
	private static final long serialVersionUID = -2070469677921953224L;

	@SuppressWarnings("all")
	private static final Logger LOGGER = Logger.getLogger("javamelody");

	private Pattern allowedAddrPattern;

	@SuppressWarnings("all")
	private transient CollectorServer collectorServer;

	/** {@inheritDoc} */
	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		Parameters.initialize(config.getServletContext());
		if (!Boolean.parseBoolean(Parameters.getParameter(Parameter.LOG))) {
			// si log désactivé dans serveur de collecte,
			// alors pas de log, comme dans webapp
			LOGGER.setLevel(Level.WARN);
		}
		// dans le serveur de collecte, on est sûr que log4j est disponible
		LOGGER.info("initialization of the collector servlet of the monitoring");
		if (Parameters.getParameter(Parameter.ALLOWED_ADDR_PATTERN) != null) {
			allowedAddrPattern = Pattern.compile(Parameters
					.getParameter(Parameter.ALLOWED_ADDR_PATTERN));
		}

		try {
			collectorServer = new CollectorServer();
		} catch (final IOException e) {
			throw new ServletException(e.getMessage(), e);
		}
	}

	/** {@inheritDoc} */
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException,
			IOException {
		final long start = System.currentTimeMillis();
		if (isAddressNotAllowed(req)) {
			LOGGER.info("Forbidden access to monitoring from " + req.getRemoteAddr());
			resp.sendError(HttpServletResponse.SC_FORBIDDEN, "Forbidden access");
			return;
		}
		final CollectorController collectorController = new CollectorController(collectorServer);
		final String application = collectorController.getApplication(req, resp);
		I18N.bindLocale(req.getLocale());
		try {
			if (application == null) {
				CollectorController.writeOnlyAddApplication(resp);
				return;
			}
			if (!collectorServer.isApplicationDataAvailable(application)) {
				resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
						"Data unavailable for the application " + application);
				return;
			}
			collectorController.doMonitoring(req, resp, application);
		} finally {
			I18N.unbindLocale();
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("monitoring from " + req.getRemoteAddr() + ", request="
						+ req.getRequestURI()
						+ (req.getQueryString() != null ? '?' + req.getQueryString() : "")
						+ ", application=" + application + " in "
						+ (System.currentTimeMillis() - start) + "ms");
			}
		}
	}

	/** {@inheritDoc} */
	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		if (isAddressNotAllowed(req)) {
			LOGGER.info("Forbidden access to monitoring from " + req.getRemoteAddr());
			resp.sendError(HttpServletResponse.SC_FORBIDDEN, "Forbidden access");
			return;
		}
		// post du formulaire d'ajout d'application à monitorer
		final String appName = req.getParameter("appName");
		final String appUrls = req.getParameter("appUrls");
		I18N.bindLocale(req.getLocale());
		final CollectorController collectorController = new CollectorController(collectorServer);
		try {
			if (appName == null || appUrls == null) {
				writeMessage(req, resp, collectorController, I18N.getString("donnees_manquantes"));
				return;
			}
			if (!appUrls.startsWith("http://") && !appUrls.startsWith("https://")) {
				writeMessage(req, resp, collectorController, I18N.getString("urls_format"));
				return;
			}
			collectorController.addCollectorApplication(appName, appUrls);
			LOGGER.info("monitored application added: " + appName);
			LOGGER.info("urls of the monitored application: " + appUrls);
			CollectorController.showAlertAndRedirectTo(resp,
					I18N.getFormattedString("application_ajoutee", appName), "?application="
							+ appName);
		} catch (final FileNotFoundException e) {
			final String message = I18N.getString("monitoring_configure");
			LOGGER.warn(message, e);
			writeMessage(req, resp, collectorController, message + '\n' + e.toString());
		} catch (final StreamCorruptedException e) {
			final String message = I18N.getFormattedString("reponse_non_comprise", appUrls);
			LOGGER.warn(message, e);
			writeMessage(req, resp, collectorController, message + '\n' + e.toString());
		} catch (final Exception e) {
			LOGGER.warn(e.toString(), e);
			writeMessage(req, resp, collectorController, e.toString());
		} finally {
			I18N.unbindLocale();
		}
	}

	private void writeMessage(HttpServletRequest req, HttpServletResponse resp,
			CollectorController collectorController, String message) throws IOException {
		collectorController.writeMessage(req, resp, collectorController.getApplication(req, resp),
				message);
	}

	private boolean isAddressNotAllowed(HttpServletRequest req) {
		return allowedAddrPattern != null
				&& !allowedAddrPattern.matcher(req.getRemoteAddr()).matches();
	}

	/** {@inheritDoc} */
	@Override
	public void destroy() {
		LOGGER.info("collector servlet stopping");
		if (collectorServer != null) {
			collectorServer.stop();
		}
		Collector.stopJRobin();
		LOGGER.info("collector servlet stopped");
		super.destroy();
	}
}
