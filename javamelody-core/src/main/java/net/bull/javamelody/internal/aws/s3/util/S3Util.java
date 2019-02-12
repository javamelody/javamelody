package net.bull.javamelody.internal.aws.s3.util;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.AbortMultipartUploadRequest;
import com.amazonaws.services.s3.model.CompleteMultipartUploadRequest;
import com.amazonaws.services.s3.model.InitiateMultipartUploadRequest;
import com.amazonaws.services.s3.model.InitiateMultipartUploadResult;
import com.amazonaws.services.s3.model.PartETag;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.amazonaws.services.s3.transfer.internal.UploadPartRequestFactory;

import net.bull.javamelody.internal.aws.s3.S3ClientFactory;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.util.MemoryUtil;

/**
 * Util class for AWS S3.
 * @author Salah Qasem 
 *
 */
public class S3Util {

	private static final long PART_SIZE = 200 * 1024 * 1024; // 200 mb

	private S3Util() {

	}

	/**
	 * upload file to AWS S3.
	 * @param file
	 * @throws Exception
	 */
	public static void upload(File file) throws Exception {

		String bucketName = System.getProperty("s3.bucketName");

		if (bucketName == null || "".equals(bucketName.trim())) {
			throw new IllegalStateException("s3 bucketName property can't be empty.");
		}

		if (getFileSizeInGiga(file) > 0.3) {
			uploadMultiPart(file, bucketName);
			return;
		}
		try {
			AmazonS3 s3Client = S3ClientFactory.INSTANCE.createClient();
			s3Client.putObject(bucketName, file.getName(), file);
			LOG.info("File " + file.getName() + " uploaded successfully to S3");
		} catch (Exception e) {
			throw e;
		}
	}

	private static void uploadMultiPart(File file, String bucketName) throws Exception {
		AmazonS3 s3Client = S3ClientFactory.INSTANCE.createClient();
		List<PartETag> partETags = new ArrayList<PartETag>();

		InitiateMultipartUploadRequest initRequest = new InitiateMultipartUploadRequest(bucketName,
				file.getName());
		InitiateMultipartUploadResult initResponse = s3Client.initiateMultipartUpload(initRequest);
		PutObjectRequest putObjectRequest = new PutObjectRequest(bucketName, file.getName(), file);
		UploadPartRequestFactory requestFactory = new UploadPartRequestFactory(putObjectRequest,
				initResponse.getUploadId(), PART_SIZE);

		try {
			while (requestFactory.hasMoreRequests()) {
				partETags.add(s3Client.uploadPart(requestFactory.getNextUploadPartRequest())
						.getPartETag());
			}
			CompleteMultipartUploadRequest compRequest = new CompleteMultipartUploadRequest(
					bucketName, file.getName(), initResponse.getUploadId(), partETags);
			s3Client.completeMultipartUpload(compRequest);
			LOG.info("File " + file.getName() + " uploaded successfully to S3");
		} catch (Exception e) {
			s3Client.abortMultipartUpload(new AbortMultipartUploadRequest(bucketName,
					file.getName(), initResponse.getUploadId()));
			throw e;
		}

	}

	private static double getFileSizeInGiga(File file) {
		return (file.length() * 1.0) / MemoryUtil.Size.GIGA_BYTE.getByteSize();
	}
}
