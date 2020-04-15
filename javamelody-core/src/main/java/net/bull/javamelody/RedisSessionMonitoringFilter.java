package net.bull.javamelody;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

/**
 * https://github.com/javamelody/javamelody/issues/906
 * @author Emeric Vernat
 */
public class RedisSessionMonitoringFilter implements Filter {
	@Override
	public void init(FilterConfig filterConfig) throws ServletException {
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		try {
			chain.doFilter(request, response);
		} finally {
			final HttpServletRequest httpRequest = (HttpServletRequest) request;
			if (httpRequest.isRequestedSessionIdValid()) {
				final HttpSession session = httpRequest.getSession(false);
				SessionListener.updateSession(session);
			}
		}
	}

	@Override
	public void destroy() {
	}
}
