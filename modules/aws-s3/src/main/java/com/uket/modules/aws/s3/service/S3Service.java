package com.uket.modules.aws.s3.service;

import com.uket.modules.aws.s3.properties.S3Properties;
import java.time.Duration;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.services.s3.presigner.S3Presigner;
import software.amazon.awssdk.services.s3.presigner.model.GetObjectPresignRequest;

@Service
@RequiredArgsConstructor
public class S3Service {

    private static final String UNIVERSITY_LOGO_FOLDER = "university-logo";
    private static final String BANNER_IMAGE_FOLDER = "banner";

    private final S3Presigner preSigner;
    private final S3Properties s3Properties;

    public String getUniversityLogo(String filename) {
        return getPreSignedUrl(UNIVERSITY_LOGO_FOLDER, filename);
    }

    public String getBannerImage(String filename) {
        return getPreSignedUrl(BANNER_IMAGE_FOLDER, filename);
    }

    private String getPreSignedUrl(String folder, String filename) {
        if (filename == null || filename.isEmpty()) {
            return null;
        }
        String url = preSigner
                .presignGetObject(getObjectPresignRequest(folder, filename))
                .url()
                .toString();

        preSigner.close();
        return url;
    }

    private GetObjectPresignRequest getObjectPresignRequest(String folder, String filename) {
        return GetObjectPresignRequest.builder()
                .signatureDuration(Duration.ofMinutes(1))
                .getObjectRequest(objectRequest ->
                        objectRequest
                                .bucket(s3Properties.bucket())
                                .key(String.join("/", folder, filename)))
                .build();
    }
}
