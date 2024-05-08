package com.uket.domain.event.repository;

import com.uket.domain.event.entity.Banner;
import com.uket.domain.event.entity.Events;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface BannerRepository extends JpaRepository<Banner,Long> {

    List<Banner> findByEvent(Events events);
}
