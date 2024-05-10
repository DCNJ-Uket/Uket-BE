package com.uket.domain.event.dto;

public record BannerDto(
        String url
) {

    public static BannerDto from(String bannerUrl) {
        return new BannerDto(bannerUrl);
    }
}
