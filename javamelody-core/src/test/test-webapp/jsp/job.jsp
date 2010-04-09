<%@ page session="false" %>
<%@page import="java.util.Random"%>
<%@page import="java.util.Date"%>
<%@page import="org.quartz.*"%>
<%@page import="org.quartz.impl.StdSchedulerFactory"%>
<%@page import="net.bull.javamelody.JobTestImpl"%>
<%
//Grab the Scheduler instance from the Factory 
Scheduler scheduler = StdSchedulerFactory.getDefaultScheduler();

// and start it off
scheduler.start();

//Define job instance
Random random = new Random();
JobDetail job = new JobDetail("job" + random.nextInt(), null, JobTestImpl.class);
job.setDescription("coucou\ncoucou");

//Define a Trigger that will fire "now"
Trigger trigger = new SimpleTrigger("trigger" + random.nextInt(), null, 100, 20 * 1000);
//Schedule the job with the trigger
scheduler.scheduleJob(job, trigger);

//Define a Trigger that will fire "later"
JobDetail job2 = new JobDetail("job" + random.nextInt(), null, JobTestImpl.class);
Trigger trigger2 = new SimpleTrigger("trigger" + random.nextInt(), null, new Date(System.currentTimeMillis() + random.nextInt(60000)));
scheduler.scheduleJob(job2, trigger2);

%>

Jobs initialized

<br/>
<a href="../index.jsp">back</a>
