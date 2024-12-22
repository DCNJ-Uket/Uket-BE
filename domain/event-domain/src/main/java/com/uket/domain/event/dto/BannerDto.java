package com.uket.domain.event.dto;

public record BannerDto(
        String title,
        String url,
        String redirectUrl
) {

    public static BannerDto of(String title, String bannerUrl, String redirectUrl) {
        return new BannerDto(title, bannerUrl, redirectUrl);
    }
}
