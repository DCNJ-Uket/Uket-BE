package com.uket.domain.event.dto;

public record BannerDto(
        String title,
        String url
) {

    public static BannerDto of(String title, String bannerUrl) {
        return new BannerDto(title, bannerUrl);
    }
}
