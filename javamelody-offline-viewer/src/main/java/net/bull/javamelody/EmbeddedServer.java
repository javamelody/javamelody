package net.bull.javamelody;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.EnumSet;
import java.util.Map;
import java.util.Set;

import org.apache.catalina.Context;
import org.apache.catalina.servlets.DefaultServlet;
import org.apache.catalina.startup.Tomcat;

import jakarta.servlet.DispatcherType;
import jakarta.servlet.FilterRegistration.Dynamic;
import jakarta.servlet.ServletContainerInitializer;
import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletException;

/**
 * Embedded http server including javamelody reports.
 */
public class EmbeddedServer {
	private static Tomcat tomcat;
	private static Path webappDir;

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
		// active le connector sinon il n'est pas actif
		tomcat.getConnector();

		// Répertoire de ressources web bidon juste parce qu'il en faut un
		webappDir = Files.createTempDirectory("javamelody-embedded-").toAbsolutePath();
		final Context context = tomcat.addContext("", webappDir.toString());
		// il faut une servlet, sinon le filtre n'est pas actif
		Tomcat.addServlet(context, "default", new DefaultServlet());
		context.addServletMappingDecoded("/", "default");

		// ServletContainerInitializer qui initialisera le filtre
		final ServletContainerInitializer servletContainerInitializer = new ServletContainerInitializer() {

			@Override
			public void onStartup(Set<Class<?>> c, ServletContext ctx) throws ServletException {
				// initialise le filtre pour activer le monitoring et pour afficher la page
				final net.bull.javamelody.MonitoringFilter monitoringFilter = new net.bull.javamelody.MonitoringFilter();
				monitoringFilter.setApplicationType("Standalone");
				final Dynamic filter = ctx.addFilter("javamelody", monitoringFilter);
				filter.addMappingForUrlPatterns(
						EnumSet.of(DispatcherType.INCLUDE, DispatcherType.REQUEST), false, "/*");
				if (parameters != null) {
					for (final Map.Entry<Parameter, String> entry : parameters.entrySet()) {
						final net.bull.javamelody.Parameter parameter = entry.getKey();
						final String value = entry.getValue();
						filter.setInitParameter(parameter.getCode(), value);
					}
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
			Files.delete(webappDir);
		}
	}
}
