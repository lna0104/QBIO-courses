"""
Write a program that calculates your final percentage and grade
in CSSE1001/7030.

Your final percentage is based on your results in MyPyTutor, 
the three assignments and the final exam.

Challenge 1: Output your grade based on your final percentage
Challenge 2: Output your grade based on the course rules
             See: http://www.courses.uq.edu.au/student_section_loader.php?section=5&profileId=92705
"""

__author__ = "Richard Thomas"
__date__ = "01/03/2021"
__copyright__ = "The University of Queensland"

another_student = "Y"

while another_student == "Y":
    mypytutor_mark = input("Please enter your MyPyTutor mark: ")
    assign1_mark = input("Please enter your mark for the first assignment: ")
    assign2_mark = input("Please enter your mark for the second assignment: ")
    assign3_mark = input("Please enter your mark for the third assignment: ")
    exam_mark = input("Please enter your mark for the final exam: ")

    # Convert the input strings to floats.
    # Assume that raw scores have been entered.
    mypytutor_mark = float(mypytutor_mark)
    assign1_mark = float(assign1_mark)
    assign2_mark = float(assign2_mark)
    assign3_mark = float(assign3_mark)
    exam_mark = float(exam_mark)

    # Calculate final results.
    normalised_exam_mark = exam_mark / 40
    final_mark = (mypytutor_mark
                + assign1_mark + assign2_mark + assign3_mark
                + exam_mark)

    print("Your final percentage in CSSE1001/7030 is", final_mark)

    # Determine the grade achieved.
    if (final_mark >= 85) and (normalised_exam_mark >= 0.80) :
        grade = 7
    elif (final_mark >= 75) and (normalised_exam_mark >= 0.70) :
        grade = 6
    elif (final_mark >= 65) and (normalised_exam_mark >= 0.60) :
        grade = 5
    elif (final_mark >= 50) and (normalised_exam_mark >= 0.45) :
        grade = 4
    elif (final_mark >= 45) and (normalised_exam_mark >= 0.40) :
        grade = 3
    elif (final_mark >= 20) and (normalised_exam_mark >= 0.2) :
        grade = 2
    else :
        grade = 1

    print("Final grade is:", grade)
    another_student = input("Do you want to continue for next student? Please type Y or N: ")
