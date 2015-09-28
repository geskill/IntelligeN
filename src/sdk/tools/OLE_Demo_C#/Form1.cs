using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace WindowsFormsApplication1
{
    public partial class Main : Form
    {

        IntelligeN.IIntelligeN2009 IntelligeNInstance;

        public Main()
        {
            InitializeComponent();
        }

        private void Main_Load(object sender, EventArgs e)
        {

        }

        private void bStartIntelligeN_Click(object sender, EventArgs e)
        {
            IntelligeNInstance = new IntelligeN.IntelligeN2009();
        }

        private void bClose_Click(object sender, EventArgs e)
        {
            if (IntelligeNInstance != null)
                IntelligeNInstance.close();

            IntelligeNInstance = null;
        }

        private void bLoadXML_Click(object sender, EventArgs e)
        {
            if (IntelligeNInstance != null)
                IntelligeNInstance.openfile(tbFileName.Text);
        }

        private void bSearchFile_Click(object sender, EventArgs e)
        {
            if (openFileDialog.ShowDialog() == DialogResult.OK)
                tbFileName.Text = openFileDialog.FileName;
        }

    }
}
