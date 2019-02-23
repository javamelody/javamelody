package net.bull.javamelody.internal.aws.s3;

import com.amazonaws.auth.DefaultAWSCredentialsProviderChain;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;

/**
 * Client Factory for AWS S3.
 * @author Salah Qasem 
 *
 */

public class S3ClientFactory {

	private static final String REGION = "region";

	public final static S3ClientFactory INSTANCE = new S3ClientFactory();

	private S3ClientFactory() {
	}

	/**
	 * Creates an AWS s3 client.
	 * @return AmazonS3 client
	 */
	public AmazonS3 createClient() {
		AmazonS3ClientBuilder clientBuilder = AmazonS3Client.builder();
		clientBuilder.withCredentials(new DefaultAWSCredentialsProviderChain());

		if (System.getProperty(REGION) != null && !"".equals(System.getProperty(REGION).trim())) {
			clientBuilder.withRegion(System.getProperty(REGION));
		} else {
			clientBuilder.withRegion(Regions.DEFAULT_REGION);
		}
		return clientBuilder.build();
	}

}
