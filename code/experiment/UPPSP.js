// function for getting responses from the UPPS-P questionnaire
var get_resp = function(data, reverse) {
  var responses = JSON.parse(data.responses);
  var resp = responses.Q0 + 1; // javaScript uses 0-based indexing. Change into 1-based indexing

  // reverse coding
  if (reverse === 1) {
    resp = 5 - resp;
  };

  return resp;

}

var scale_1 = ["Agree strongly", "Agree some", "Disagree some", "Disagree strongly"];

 // questions
var Question1 = {
  type: 'survey-likert',
  questions: [{
    prompt: "I usually like to see things through to the end.",
    labels: scale_1,
    required: true,
  }, ],
  on_finish: function(data) {
    data.reverse = 0;
    data.factor = "Perseverance";

    data.resp = get_resp(data, data.reverse);
  }
}

var Question2 = {
  type: 'survey-likert',
  questions: [{
    prompt: "My thinking is usually careful and purposeful.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 0;
    data.factor = "Premeditation";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question3 = {
  type: 'survey-likert',
  questions: [{
    prompt: "When I am in great mood, I tend to get into situations that could cause me problems.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 1;
    data.factor = "Positive Urgency";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question4 = {
  type: 'survey-likert',
  questions: [{
    prompt: "Unfinished tasks really bother me.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 0;
    data.factor = "Perseverance";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question5 = {
  type: 'survey-likert',
  questions: [{
    prompt: "I like to stop and think things over before I do them.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 0;
    data.factor = "Premeditation";

    data.resp = get_resp(data, data.reverse);
  }
};


var Question6 = {
  type: 'survey-likert',
    questions: [{
    prompt: "When I feel bad, I quite often do things I later regret in order to make myself feel better now.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 1;
    data.factor = "Negative Urgency";

    data.resp = get_resp(data, data.reverse);
  }
};


var Question7 = {
  type: 'survey-likert',
    questions: [{
    prompt: "Once I get going on something I hate to stop.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 0;
    data.factor = "Perseverance";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question8 = {
  type: 'survey-likert',
    questions: [{
    prompt: "Sometimes when I feel bad, I can't seem to stop what I am doing even though it is making me feel worse.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 1;
    data.factor = "Negative Urgency";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question9 = {
  type: 'survey-likert',
    questions: [{
    prompt: "I quite enjoy taking risks.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 1;
    data.factor = "Sensation Seeking";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question10 = {
  type: 'survey-likert',
    questions: [{
    prompt: "I tend to lose control when I am in a great mood.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 1;
    data.factor = "Positive Urgency";

    data.resp = get_resp(data, data.reverse);
  }
};


var Question11 = {
  type: 'survey-likert',
    questions: [{
    prompt: "I finish what I start.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 0;
    data.factor = "Perseverance";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question12 = {
  type: 'survey-likert',
    questions: [{
    prompt: "I tend to value and follow a rational, 'sensible' approach to things .",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 0;
    data.factor = "Premeditation";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question13 = {
  type: 'survey-likert',
    questions: [{
    prompt: "When I am upset I often act without thinking.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 1;
    data.factor = "Negative Urgency";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question14 = {
  type: 'survey-likert',
    questions: [{
    prompt: "I welcome new and exciting experiences and sensations, even if they are a little frightening and unconventional.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 1;
    data.factor = "Sensation Seeking";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question15 = {
  type: 'survey-likert',
    questions: [{
    prompt: "When I feel rejected, I will often say things that I later regret.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 1;
    data.factor = "Negative Urgency";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question16 = {
  type: 'survey-likert',
    questions: [{
    prompt: "I would like to learn to fly an airplane.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 1;
    data.factor = "Sensation Seeking";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question17 = {
  type: 'survey-likert',
    questions: [{
    prompt: "Others are shocked or worried about the things I do when I am feeling very excited.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 1;
    data.factor = "Positive Urgency";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question18 = {
  type: 'survey-likert',
    questions: [{
    prompt: "I would enjoy the sensation of skiing very fast down a high mountain slope.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 1;
    data.factor = "Sensation Seeking";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question19 = {
  type: 'survey-likert',
    questions: [{
    prompt: "I usually think carefully before doing anything.",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 0;
    data.factor = "Premeditation";

    data.resp = get_resp(data, data.reverse);
  }
};

var Question20 = {
  type: 'survey-likert',
    questions: [{
    prompt: "I tend to act without thinking when I am really excited .",
    labels: scale_1,
    required: true
  }, ],
  on_finish: function(data) {
    data.reverse = 1;
    data.factor = "Positive Urgency";

    data.resp = get_resp(data, data.reverse);
  }
};


var UPPSP_items = [Question1, Question2, Question3, Question4, Question5, Question6, Question7, Question8, Question9, Question10,
  Question11, Question12, Question13, Question14, Question15, Question16, Question17, Question18, Question19, Question20];
