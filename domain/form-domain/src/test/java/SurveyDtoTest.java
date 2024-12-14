import com.uket.domain.form.dto.SurveyDto;
import com.uket.domain.form.entity.DropdownForm;
import com.uket.domain.form.entity.Survey;
import com.uket.domain.form.entity.TextForm;
import java.util.List;
import org.junit.jupiter.api.Test;

public class SurveyDtoTest {
    @Test
    void test() {
        TextForm form1 = new TextForm(1L, "test");
        DropdownForm form2 = new DropdownForm(2L, "test", List.of("option1", "option2"));
        Survey survey = new Survey(1L, List.of(form1, form2));
        System.out.println(SurveyDto.from(survey));
    }
}
