/*
 * Copyright 2008-2019 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.bull.javamelody.internal.publish;

import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

import net.bull.javamelody.Parameter;
import software.amazon.awssdk.services.cloudwatch.CloudWatchClient;
import software.amazon.awssdk.services.cloudwatch.model.Dimension;
import software.amazon.awssdk.services.cloudwatch.model.MetricDatum;
import software.amazon.awssdk.services.cloudwatch.model.PutMetricDataRequest;

/**
 * Publish chart data to <a href='https://aws.amazon.com/cloudwatch/'>AWS CloudWatch</a>.
 * @author Emeric Vernat
 */
class CloudWatch extends MetricsPublisher {
	private final String cloudWatchNamespace;
	private final CloudWatchClient cloudWatchClient;
	private final String prefix;
	private final List<Dimension> dimensions = new ArrayList<>();

	private final List<MetricDatum> buffer = new ArrayList<>();
	private long lastTime;
	private Instant lastTimestamp;

	CloudWatch(CloudWatchClient cloudWatchClient, String cloudWatchNamespace, String prefix,
			String application, String hostName) {
		super();
		assert cloudWatchClient != null;
		assert cloudWatchNamespace != null && !cloudWatchNamespace.startsWith("AWS/")
				&& !cloudWatchNamespace.isEmpty() && cloudWatchNamespace.length() <= 255;
		assert prefix != null;
		assert !application.isEmpty() && application.length() <= 255;
		assert !hostName.isEmpty() && hostName.length() <= 255;

		this.cloudWatchClient = cloudWatchClient;
		this.cloudWatchNamespace = cloudWatchNamespace;
		this.prefix = prefix;
		// A dimension is like a tag which can be used to filter metrics in the CloudWatch UI.
		// Name and value of dimensions have min length 1 and max length 255.
		dimensions.add(Dimension.builder().name("application").value(application).build());
		dimensions.add(Dimension.builder().name("hostname").value(hostName).build());
		// note: to add other dimensions (max 30 dimensions), we could call
		// new URL("http://instance-data/latest/meta-data/instance-id").openStream(),
		// or /ami-id, /placement/availability-zone, /instance-type, /local-hostname, /local-ipv4, /public-hostname, /public-ipv4
		// see http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html
		// except that it would not work from the collector server
	}

	/**
	 * New CloudWatch with DefaultAWSCredentialsProviderChain (and DefaultAwsRegionProviderChain) configured either by :
	 * <ul>
	 *   <li>Environment Variables -
	 *      <code>AWS_ACCESS_KEY_ID</code> and <code>AWS_SECRET_ACCESS_KEY</code>
	 *      (RECOMMENDED since they are recognized by all the AWS SDKs and CLI except for .NET),
	 *      or <code>AWS_ACCESS_KEY</code> and <code>AWS_SECRET_KEY</code> (only recognized by Java SDK)
	 *   </li>
	 *   <li>Java System Properties - aws.accessKeyId and aws.secretKey</li>
	 *   <li>Credential profiles file at the default location (~/.aws/credentials) shared by all AWS SDKs and the AWS CLI</li>
	 *   <li>Credentials delivered through the Amazon EC2 container service if AWS_CONTAINER_CREDENTIALS_RELATIVE_URI" environment variable is set
	 *   and security manager has permission to access the variable,</li>
	 *   <li>Instance profile credentials delivered through the Amazon EC2 metadata service</li>
	 * </ul>
	 * (idem for AWS region)
	 *
	 * @param cloudWatchNamespace CloudWatch Namespace such as "MyCompany/MyAppDomain"
	 * 		(Namespace of Amazon EC2 is "AWS/EC2", but "AWS/*" is reserved for AWS products)
	 * @param prefix Prefix such as "javamelody."
	 * @param application Application such as /testapp
	 * @param hostName Hostname such as www.host.com@11.22.33.44
	 */
	CloudWatch(String cloudWatchNamespace, String prefix, String application, String hostName) {
		this(CloudWatchClient.create(), cloudWatchNamespace, prefix, application, hostName);
	}

	static CloudWatch getInstance(String contextPath, String hostName) {
		final String cloudWatchNamespace = Parameter.CLOUDWATCH_NAMESPACE.getValue();
		if (cloudWatchNamespace != null) {
			assert contextPath != null;
			assert hostName != null;
			if (cloudWatchNamespace.startsWith("AWS/")) {
				// http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_PutMetricData.html
				throw new IllegalArgumentException(
						"CloudWatch namespaces starting with \"AWS/\" are reserved for use by AWS products.");
			}
			final String prefix = "javamelody.";
			// contextPath est du genre "/testapp"
			// hostName est du genre "www.host.com"
			return new CloudWatch(cloudWatchNamespace, prefix, contextPath, hostName);
		}
		return null;
	}

	@Override
	public void addValue(String metric, double value) {
		assert metric != null;
		final long timeInSeconds = System.currentTimeMillis() / 1000;
		if (lastTime != timeInSeconds) {
			lastTimestamp = Instant.now();
			lastTime = timeInSeconds;
		}
		// http://docs.amazonwebservices.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html
		// In theory, large values are rejected, but the maximum is so large that we don't bother to verify.
		final MetricDatum metricDatum = MetricDatum.builder().metricName(prefix + metric)
				.dimensions(dimensions).timestamp(lastTimestamp).value(value).build();
		//.unit("None")
		synchronized (buffer) {
			buffer.add(metricDatum);
		}
	}

	@Override
	public void send() throws IOException {
		final List<MetricDatum> datumList;
		synchronized (buffer) {
			datumList = new ArrayList<>(buffer);
			buffer.clear();
		}
		// Each PutMetricData request is limited to 1 MB in size for HTTP POST requests.
		// The metricData collection is limited to 1000 different metrics, so we are OK without partitioning the request.
		final PutMetricDataRequest request = PutMetricDataRequest.builder()
				.namespace(cloudWatchNamespace).metricData(datumList).build();
		try {
			cloudWatchClient.putMetricData(request);
		} catch (final Exception e) {
			// pas catch (CloudWatchException) sinon ClassNotFoundException dans Jenkins par ex
			throw new IOException("Error connecting to AWS CloudWatch", e);
		}
	}

	@Override
	public void stop() {
		cloudWatchClient.close();
	}
}
