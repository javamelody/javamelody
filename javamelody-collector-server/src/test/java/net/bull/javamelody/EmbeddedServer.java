package net.bull.javamelody;

import org.eclipse.jetty.ee10.servlet.ServletHolder;
import org.eclipse.jetty.server.Handler;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.InetAccessHandler;
import org.eclipse.jetty.ee10.webapp.WebAppContext;

/**
 * Embedded http server including javamelody reports.
 */
public class EmbeddedServer {
	public static void main(String[] args) throws Exception {
		start(8080);
	}

	/**
	 * Start the server with a http port.
	 * @param port Http port
	 * @throws Exception e
	 */
	public static void start(int port) throws Exception {
		// Init jetty
		final Server server = new Server(port);
		final InetAccessHandler inetAccessHandler = new InetAccessHandler();
		server.setHandler(inetAccessHandler);

		final Handler.Sequence handlers = new Handler.Sequence();
		inetAccessHandler.setHandler(handlers);

		final WebAppContext webapp = new WebAppContext();
		webapp.setContextPath("/");
		webapp.setWar("javamelody-collector-server/src/main/webapp");

		// servlet of the javamelody collector server
		final ServletHolder servletHolder = new ServletHolder(new CollectorServlet());
		webapp.addServlet(servletHolder, "/*");

		handlers.addHandler(webapp);

		server.start();
		server.join();
	}
}
