package net.bull.javamelody;

import java.io.File;
import java.nio.file.Files;
import java.util.EnumSet;
import java.util.Map;

import org.apache.catalina.Context;
import org.apache.catalina.servlets.DefaultServlet;
import org.apache.catalina.startup.Tomcat;

import jakarta.servlet.DispatcherType;
import jakarta.servlet.FilterRegistration.Dynamic;
import jakarta.servlet.ServletContainerInitializer;

/**
 * Embedded http server including javamelody reports.
 */
public class EmbeddedServer {
	private static Tomcat tomcat;
	private static File baseDir;
	private static File webappDir;

	/**
	 * Start the server with a http port and optional javamelody parameters.
	 * @param port Http port
	 * @param parameters Optional javamelody parameters
	 * @throws Exception e
	 */
	public static void start(final int port, final Map<Parameter, String> parameters)
			throws Exception {
		// Init embedded tomcat
		tomcat = new Tomcat();
		tomcat.setPort(port);
		baseDir = Files.createTempDirectory("javamelody-embedded-tomcat-").toFile();
		tomcat.setBaseDir(baseDir.getAbsolutePath());
		// active le connector sinon il n'est pas actif
		tomcat.getConnector();

		// Répertoire de ressources web bidon juste parce qu'il en faut un
		webappDir = Files.createTempDirectory("javamelody-embedded-").toFile();
		final Context context = tomcat.addContext("", webappDir.getAbsolutePath());
		// il faut une servlet, sinon le filtre n'est pas actif
		Tomcat.addServlet(context, "default", new DefaultServlet());
		context.addServletMappingDecoded("/", "default");

		// ServletContainerInitializer qui initialisera le filtre
		final ServletContainerInitializer servletContainerInitializer = (c, ctx) -> {
			// initialise le filtre pour activer le monitoring et pour afficher la page
			final MonitoringFilter monitoringFilter = new MonitoringFilter();
			monitoringFilter.setApplicationType("Standalone");
			final Dynamic filter = ctx.addFilter("javamelody", monitoringFilter);
			filter.addMappingForUrlPatterns(
					EnumSet.of(DispatcherType.INCLUDE, DispatcherType.REQUEST), false, "/*");
			if (parameters != null) {
				for (final Map.Entry<Parameter, String> entry : parameters.entrySet()) {
					final Parameter parameter = entry.getKey();
					final String value = entry.getValue();
					filter.setInitParameter(parameter.getCode(), value);
				}
			}
		};
		context.addServletContainerInitializer(servletContainerInitializer, null);

		tomcat.start();
	}

	/**
	 * Stop the server.
	 * @throws Exception e
	 */
	public static void stop() throws Exception {
		if (tomcat != null) {
			tomcat.stop();
			deleteRecursive(baseDir);
			deleteRecursive(webappDir);
		}
	}

	private static void deleteRecursive(final File directory) {
		final File[] files = directory.listFiles();
		if (files != null) {
			for (final File file : files) {
				if (file.isDirectory()) {
					deleteRecursive(file);
				}
				file.delete();
			}
		}
	}
}
