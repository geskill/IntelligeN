namespace WindowsFormsApplication1
{
    partial class Main
    {
        /// <summary>
        /// Erforderliche Designervariable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Verwendete Ressourcen bereinigen.
        /// </summary>
        /// <param name="disposing">True, wenn verwaltete Ressourcen gelöscht werden sollen; andernfalls False.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Vom Windows Form-Designer generierter Code

        /// <summary>
        /// Erforderliche Methode für die Designerunterstützung.
        /// Der Inhalt der Methode darf nicht mit dem Code-Editor geändert werden.
        /// </summary>
        private void InitializeComponent()
        {
            this.bStartIntelligeN = new System.Windows.Forms.Button();
            this.bClose = new System.Windows.Forms.Button();
            this.bLoadXML = new System.Windows.Forms.Button();
            this.tbFileName = new System.Windows.Forms.TextBox();
            this.bSearchFile = new System.Windows.Forms.Button();
            this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.SuspendLayout();
            // 
            // bStartIntelligeN
            // 
            this.bStartIntelligeN.Location = new System.Drawing.Point(12, 12);
            this.bStartIntelligeN.Name = "bStartIntelligeN";
            this.bStartIntelligeN.Size = new System.Drawing.Size(97, 23);
            this.bStartIntelligeN.TabIndex = 0;
            this.bStartIntelligeN.Text = "Start IntelligeN";
            this.bStartIntelligeN.UseVisualStyleBackColor = true;
            this.bStartIntelligeN.Click += new System.EventHandler(this.bStartIntelligeN_Click);
            // 
            // bClose
            // 
            this.bClose.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.bClose.Location = new System.Drawing.Point(384, 12);
            this.bClose.Name = "bClose";
            this.bClose.Size = new System.Drawing.Size(101, 23);
            this.bClose.TabIndex = 1;
            this.bClose.Text = "Close IntelligeN";
            this.bClose.UseVisualStyleBackColor = true;
            this.bClose.Click += new System.EventHandler(this.bClose_Click);
            // 
            // bLoadXML
            // 
            this.bLoadXML.Location = new System.Drawing.Point(34, 41);
            this.bLoadXML.Name = "bLoadXML";
            this.bLoadXML.Size = new System.Drawing.Size(75, 23);
            this.bLoadXML.TabIndex = 2;
            this.bLoadXML.Text = "Load XML";
            this.bLoadXML.UseVisualStyleBackColor = true;
            this.bLoadXML.Click += new System.EventHandler(this.bLoadXML_Click);
            // 
            // tbFileName
            // 
            this.tbFileName.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.tbFileName.Location = new System.Drawing.Point(115, 43);
            this.tbFileName.Name = "tbFileName";
            this.tbFileName.Size = new System.Drawing.Size(235, 20);
            this.tbFileName.TabIndex = 3;
            // 
            // bSearchFile
            // 
            this.bSearchFile.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.bSearchFile.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.bSearchFile.Location = new System.Drawing.Point(356, 41);
            this.bSearchFile.Name = "bSearchFile";
            this.bSearchFile.Size = new System.Drawing.Size(26, 23);
            this.bSearchFile.TabIndex = 4;
            this.bSearchFile.TabStop = false;
            this.bSearchFile.Text = "...";
            this.bSearchFile.UseVisualStyleBackColor = true;
            this.bSearchFile.Click += new System.EventHandler(this.bSearchFile_Click);
            // 
            // openFileDialog
            // 
            this.openFileDialog.FileName = "openFileDialog";
            // 
            // Main
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(497, 262);
            this.Controls.Add(this.bSearchFile);
            this.Controls.Add(this.tbFileName);
            this.Controls.Add(this.bLoadXML);
            this.Controls.Add(this.bClose);
            this.Controls.Add(this.bStartIntelligeN);
            this.Name = "Main";
            this.Text = "OLE Demo";
            this.Load += new System.EventHandler(this.Main_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button bStartIntelligeN;
        private System.Windows.Forms.Button bClose;
        private System.Windows.Forms.Button bLoadXML;
        private System.Windows.Forms.TextBox tbFileName;
        private System.Windows.Forms.Button bSearchFile;
        private System.Windows.Forms.OpenFileDialog openFileDialog;
    }
}

