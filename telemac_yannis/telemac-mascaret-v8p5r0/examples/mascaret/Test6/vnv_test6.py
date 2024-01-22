
"""
Validation script for test6
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 0
        self.tags = ['mascaret']

    def _pre(self):
        """
        Defining the studies
        """

        # Test6 transcritical kernel, large mesh
        self.add_study('vnv_1',
                       'mascaret',
                       'mascaret_large.xcas')


        # Test6 transcritical kernel, fine mesh
        self.add_study('vnv_2',
                       'mascaret',
                       'mascaret_fin.xcas')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """

