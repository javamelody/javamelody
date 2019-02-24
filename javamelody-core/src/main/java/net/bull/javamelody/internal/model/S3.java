package net.bull.javamelody.internal.model;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import com.amazonaws.services.s3.model.AbortMultipartUploadRequest;
import com.amazonaws.services.s3.model.CompleteMultipartUploadRequest;
import com.amazonaws.services.s3.model.InitiateMultipartUploadRequest;
import com.amazonaws.services.s3.model.InitiateMultipartUploadResult;
import com.amazonaws.services.s3.model.PartETag;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.amazonaws.services.s3.transfer.internal.UploadPartRequestFactory;

import net.bull.javamelody.internal.common.LOG;

/**
 * Upload to AWS S3.
 * @author Salah Qasem
 */
final class S3 {
	private static final long MINIMUM_SIZE_FOR_MULTIPART = 300 * 1024 * 1024; // 300 mb
	private static final long PART_SIZE = 200 * 1024 * 1024; // 200 mb

	private S3() {
		super();
	}

	static void upload(File file, String bucketName) throws IOException {
		final AmazonS3 s3Client = AmazonS3ClientBuilder.defaultClient();
		if (file.length() > MINIMUM_SIZE_FOR_MULTIPART) {
			// multipart upload
			final List<PartETag> partETags = new ArrayList<PartETag>();
			final InitiateMultipartUploadResult initResponse = s3Client.initiateMultipartUpload(
					new InitiateMultipartUploadRequest(bucketName, file.getName()));
			final String uploadId = initResponse.getUploadId();
			final UploadPartRequestFactory requestFactory = new UploadPartRequestFactory(
					new PutObjectRequest(bucketName, file.getName(), file), uploadId, PART_SIZE);
			try {
				while (requestFactory.hasMoreRequests()) {
					partETags.add(s3Client.uploadPart(requestFactory.getNextUploadPartRequest())
							.getPartETag());
				}
				final CompleteMultipartUploadRequest compRequest = new CompleteMultipartUploadRequest(
						bucketName, file.getName(), uploadId, partETags);
				s3Client.completeMultipartUpload(compRequest);
			} catch (final Exception e) {
				s3Client.abortMultipartUpload(
						new AbortMultipartUploadRequest(bucketName, file.getName(), uploadId));
				throw new IOException(e);
			}
		} else {
			try {
				s3Client.putObject(bucketName, file.getName(), file);
			} catch (final Exception e) {
				throw new IOException(e);
			}
		}
		LOG.info("File " + file.getName() + " uploaded successfully to S3");
	}
}
