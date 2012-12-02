<%@ page session="false" %>
<% 
long sleep = Math.round(1000 * Math.random());
Thread.sleep(sleep);
out.print("Waited " + sleep + "ms");
%>
<br/>
<a href="../index.jsp">back</a>