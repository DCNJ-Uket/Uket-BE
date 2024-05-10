package com.uket.domain.event.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.dto.ShowDto;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.event.exception.EventException;
import com.uket.domain.event.repository.ShowRepository;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class ShowService {

    private final ShowRepository showRepository;

    public List<ShowDto> findByEvent(Events event) {
        List<Shows> shows = showRepository.findByEvent(event);

        return shows.stream().map(ShowDto::from).toList();
    }

    public Shows findById(Long showId) {
        return showRepository.findById(showId)
                .orElseThrow(() -> new EventException(ErrorCode.NOT_FOUND_SHOW));
    }
}
