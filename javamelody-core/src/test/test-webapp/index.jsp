<%@ page session="false" %>
<html><head><title>Webapp de test JavaMelody</title></head><body>
<h2>Webapp de test JavaMelody</h2>
<b>Statistics http</b><br/>
<a href="jsp/fast.jsp">fast page</a><br/>
<a href="jsp/slow.jsp">slow page</a><br/>
<a href="jsp/slower.jsp">slower page</a><br/>
<a href="jsp/slowest.jsp">slowest page</a><br/>
<a href="lorem_ipsum.txt?random=<%=  + Math.round(1000 * Math.random()) %>">lorem_ipsum.txt (ramdom)</a><br/>
<a href="notfound">not found</a><br/>
<br/>
<a href="jsp/sql.jsp">statistics sql</a><br/>
<a href="jsp/directSql.jsp">statistics sql via driver</a><br/>
<a href="jsp/spring.jsp">statistics spring</a><br/>
<a href="jsp/guice.jsp">statistics guice</a><br/>
<a href="jsp/service.jsp">statistics services</a><br/>
<a href="jsp/jspDispatch.jsp">statistics jsp dispatch</a><br/>
<a href="jsp/error.jsp">statistics error</a><br/>
<a href="jsp/errorCompile.jsp">statistics error (compile)</a><br/>
<a href="jsp/log.jsp">statistics log</a><br/>
<a href="jsp/needsSession.jsp">session init</a><br/>
<a href="jsp/cache.jsp">cache</a><br/>
<a href="jsp/job.jsp">job</a><br/>
<br/>
<b>Reports</b><br/>
<a href="monitoring">javamelody</a><br/>
<a href="monitoringServer">javamelody par serveur de collecte</a><br/>
</body></html>
