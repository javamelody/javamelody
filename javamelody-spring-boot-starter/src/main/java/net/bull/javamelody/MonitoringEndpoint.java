package net.bull.javamelody;

import java.io.IOException;
import java.util.Collections;
import java.util.Enumeration;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.boot.actuate.endpoint.annotation.Endpoint;
import org.springframework.boot.actuate.endpoint.annotation.ReadOperation;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

/**
 * When enabled, management endpoint for /monitoring reports on the management http port instead of the application http port.
 * @author Anders Båtstrand, breneb, Emeric Vernat
 */
@Endpoint(id = "monitoring")
// http path is /actuator/{id}
public class MonitoringEndpoint {
	private final ReportServlet reportServlet;

	/**
	 * Constructor.
	 * @param servletContext ServletContext
	 */
	public MonitoringEndpoint(ServletContext servletContext) {
		reportServlet = new ReportServlet();
		final ServletConfig servletConfig = new ServletConfig() {
			// only getServletContext() will be used by ReportServlet
			@Override
			public ServletContext getServletContext() {
				return servletContext;
			}

			@Override
			public String getServletName() {
				return MonitoringEndpoint.class.getName();
			}

			@Override
			public String getInitParameter(String name) {
				return null;
			}

			@Override
			public Enumeration<String> getInitParameterNames() {
				return Collections.emptyEnumeration();
			}
		};
		reportServlet.init(servletConfig);
	}

	/**
	 * Display a report page.
	 * @throws ServletException e
	 * @throws IOException e
	 */
	@ReadOperation
	public void report() throws ServletException, IOException {
		final ServletRequestAttributes currentRequestAttributes = (ServletRequestAttributes) RequestContextHolder
				.currentRequestAttributes();
		final HttpServletRequest httpServletRequest = currentRequestAttributes.getRequest();
		final HttpServletResponse httpResponse = currentRequestAttributes.getResponse();

		reportServlet.service(httpServletRequest, httpResponse);
	}
}
