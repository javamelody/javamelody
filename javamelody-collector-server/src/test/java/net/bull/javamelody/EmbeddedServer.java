package net.bull.javamelody;

import org.eclipse.jetty.server.Handler;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.ContextHandlerCollection;
import org.eclipse.jetty.server.handler.HandlerCollection;
import org.eclipse.jetty.server.handler.RequestLogHandler;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;

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
		final ContextHandlerCollection contexts = new ContextHandlerCollection();
		final ServletContextHandler context = new ServletContextHandler(contexts, "/");

		// servlet of the javamelody collector server
		final ServletHolder servletHolder = new ServletHolder(new CollectorServlet());

		context.addServlet(servletHolder, "/*");

		final RequestLogHandler requestLogHandler = new RequestLogHandler();
		contexts.addHandler(requestLogHandler);

		final HandlerCollection handlers = new HandlerCollection();
		handlers.setHandlers(new Handler[] { contexts });
		server.setHandler(handlers);

		server.start();
	}
}
